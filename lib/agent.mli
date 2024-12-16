module type MarkovCompressorType = sig
  type state
end

module type RewardType = sig
  type t
  type state

  val fn : state * state -> t option
end

(** A policy for infering an [action] and an [observer] given a [state] and optionally a [reward]. [RLPolicyType] is the input signature of the functor [Make]. [RLPolicyType] is the {b minimal} interface that must be provided by an input to the functor [Make], i.e. a policy may be of type [ComplexPolicy] which exposes an interface that is a superset of the [RLPolicyType] interface. *)
module type RLPolicyType = sig
  type t
  (** The policy. *)

  val init : unit -> t
  (** Initialise an [RLPolicyType.t]. *)

  type state
  (** The state of a Markov Decision Process. *)

  type reward
  (** The reward type used by the policy. E.g. [int] in the case of scalar integer reward. *)

  type observer = unit -> state
  (** A function returning a [(state, reward option)] pair. The function may intentionally block in order to delay the next observation, i.e. this function controls (dynamically) the rate at which the MDP progresses in time, relative to the progression of the underlying system. *)

  val init_observer : observer
  (** An initial observer. Upon execution the returned [(state, reward option)] may be passed as an argument to [infer] in order to produce subsequent [observers]. *)

  type action = unit -> unit
  (** An effect to be carried in the environment. *)

  type inference = {
    action : action;
    observer : observer;
    policy : t;
  }
  (** Returned by [infer]. Contains an [action], an [observer], and a new [policy]. In this way the policy controls how (and therefore when) the next observation is obtained (e.g. after a desired delay). Returning a policy at each inference facilitates functionally pure policy updates. *)

  val infer : t -> state * reward option -> inference
  (** [infer p (s,r)] applies the policy [p] of type [RLPolicyType.t] to the state [s]. An [inference] record is returned, containing an [action], an [observer] and an [RLPolicyType.t].

  If [r] is [None] then [infer] should carry out inference {b only} (no optimisation), and the returned policy is expected to be unchanged. If [r] is [Some reward] then there is an expectation that the returned policy will be a new policy that has been optimised to maximise reward.

  The interface allows for a more flexible interpretation of these suggestions if desired. *)
end

(** An MDP Agent. The output signature of the functor [Make]. *)
module type Agent = sig
  type policy
  (** The Agent's policy. As argument to the [act] function, the policy determines how the Agent i) obtains the state of the MDP, ii) decides what action to take, and iii) how the action is executed on the process. *)

  val act : policy -> 'a
  (** [act policy] is an infinite (tail-recursive) loop modelling an MDP process. Each iteration of the loop involves i) measuring the state of the MDP, ii) infering an action to take, iii) infering a method to next measure the state of the MDP, iv) executing the action, and v) repeating from i). *)
end

(** A functor. [Make P] returns an [Actor] module that is parameterised by the policy module [P]. [P] must have a type that {i includes} the interface [PolicyType] (e.g. it may be of type [PolicyType] or a {i 'super-type'} of [PolicyType]). 

This functor should implement the [act] function as follows:

1. Use the policy module [P] to obtain an initial observer ([PolicyType.init_observer]). 

2. Commence an infinite loop in which the Actor's [policy] and a [P.observer] are used to:

  i) Obtain an observation of type [P.state] by executing the [P.observer] function.

  ii) Execute [P.infer policy state] to determine a [P.action], a new [P.observer] and a new [policy].

  iii) Execute the action.

  iv) Re-enter the loop with the new [policy] and the new [P.observer]. *)
module Make
    (MarkovCompressor : MarkovCompressorType)
    (Reward : RewardType)
    (Policy : RLPolicyType
                with type state = MarkovCompressor.state
                with type reward = Reward.t) : Agent with type policy = Policy.t
