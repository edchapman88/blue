(** Exposes the functor [Agent.Make] **)

(** Handle the continuous-time stream of information from a system and compress the information into a Markovian state representation such that the sequence of states returned by sequential calls to [observe] have the Markov property. *)
module type MarkovCompressorType = sig
  type state

  val observe : unit -> state
end

(** A reward function which is a map from a [state] to a [Reward.t option]. *)
module type RewardType = sig
  type t
  type state

  val fn : state -> t
  (** [Reward.fn s] is the [Reward.t] associated with the MDP state [s]. *)
end

(** A policy for infering an [action] and an [observer] given a [state] and optionally a [reward]. *)
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

  val infer : t -> state * reward -> inference
  (** [infer p (s,r)] applies the policy [p] of type [RLPolicyType.t] to the state [s]. An [inference] record is returned, containing an [action], an [observer] and an [RLPolicyType.t].

  The returned policy may be equivalent to the input policy (no policy update) or it may be a new policy, having undergone an optimisation step. *)
end

(** An MDP Agent. The output signature of the functor [Make]. *)
module type S = sig
  type policy
  (** The Agent's policy. As argument to the [act] function, the policy determines how the Agent i) obtains the state of the MDP, ii) decides what action to take, and iii) how the action is executed on the process. *)

  val init_policy : unit -> policy
  (** Initialise the agent's policy. *)

  val act : policy -> 'a
  (** [act policy] is an infinite (tail-recursive) loop modelling an MDP process. Each iteration of the loop involves i) measuring the state of the MDP, ii) infering an action to take, iii) infering a method to next measure the state of the MDP, iv) executing the action, and v) repeating from i). *)
end

(** A functor. [Make MarkovCompressor Reward Policy] returns an [Agent] module. For example, [Policy] must be a type that {i includes} the interface [RLPolicyType] (e.g. it may be of type [RLPolicyType] or a {i 'super-type'} of [RLPolicyType]). 

This functor should implement the [act] function as follows:

1. Use the policy module to obtain an initial observer ([RLPolicyType.init_observer]). 

2. Commence an infinite loop in which the Agent's [policy] and a [Policy.observer] are used to:

  i) Obtain an observation of type [Policy.state] by executing the [Policy.observer] function.

  ii) Execute [Policy.infer policy state] to determine a [Policy.action], a new [Policy.observer] and a new [policy].

  iii) Execute the action.

  iv) Re-enter the loop with the new [policy] and the new [Policy.observer]. *)
module Make
    (MarkovCompressor : MarkovCompressorType)
    (Reward : RewardType with type state = MarkovCompressor.state)
    (Policy : RLPolicyType
                with type state = MarkovCompressor.state
                with type reward = Reward.t) : S with type policy = Policy.t
