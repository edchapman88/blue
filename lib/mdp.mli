(** A policy for infering an [action] and an [observer] given a [state]. [PolicyType] is the input signature of the functor [Make]. [PolicyType] is the {b minimal} interface that must be provided by an input to the functor [Make], i.e. a policy may be of type [ComplexPolicy] which exposes an interface that is a superset of the [PolicyType] interface. *)
module type PolicyType = sig
  type t
  (** The policy. *)

  val init : unit -> t
  (** Initialise a [Policy.t]. *)

  type state
  (** The state (of an MDP). As input to [infer], [state] may optionally included a reward signal associated with the state transition from the previous state. A reward signal may be useful to the implementer of [infer] in order to return an updated policy in the returned record ([inference]). *)

  type observer = unit -> state
  (** A function returning a [state]. The function may intentionally block in order to delay the next observation, i.e. this function controls (dynamically) the rate at which the MDP progresses in time, relative to the progression of the underlying system. *)

  val init_observer : observer
  (** An initial observer. Upon execution the returned [state] may be passed as an argument to [infer] in order to produce subsequent [observers]. *)

  type action = unit -> unit
  (** An effect to be carried in the environment. *)

  type inference = {
    action : action;
        (** For the Markov Chain assumption to hold the action should 'cause effect' sooner than the [observer] returns the next [state]. *)
    observer : observer;
    policy : t;
  }
  (** Returned by [infer]. Contains an [action], an [observer], and a new [policy]. In this way the policy controls how (and therefore when) the next observation is obtained (e.g. after a desired delay). Returning a policy at each inference facilitates functionally pure policy updates. *)

  val infer : t -> state -> inference
  (** [infer p s] applies the policy [p] of type [PolicyType.t] to the state [s]. An [inference] record is returned, containing an [action], an [observer] and a new [policy]. *)
end

(** An MDP Actor. The output signature of the functor [Make]. *)
module type Actor = sig
  type policy
  (** The Actor's policy. As argument to the [act] function, the policy determines how the Actor i) obtains the state of the MDP, ii) decides what action to take, and iii) how the action is executed on the process. *)

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
module Make (P : PolicyType) : Actor with type policy = P.t
