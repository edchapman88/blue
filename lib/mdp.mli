(** A policy for infering an [action] and an [observer] given a [state]. [PolicyType] is the input signature of the functor [Make]. *)
module type PolicyType = sig
  type t
  (** The policy. *)

  val init : unit -> t
  (** Initialise a [Policy.t]. *)

  type state
  (** The state (of an MDP). As input to [infer], [state] may optionally included a reward signal associated with the state transition from the previous state. A reward signal may be useful to the implementer of [infer] in order to return an updated policy in the returned record ([inference]). *)

  type observer = unit -> state
  (** A function returning a [state]. *)

  val init_observer : observer

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
  module Policy : PolicyType
  (** The Actor's policy. Responsible for infering an [action] and an [observer for the next step of the MDP. *)

  type state = Policy.state
  type observer = Policy.observer

  type action = Policy.action
  (** Whilst [action] and [observer] are called sequentially (rather than concurrently) in the [act] loop, the implementing policy must ensure that [action] 'causes effect' on the system sooner than the subsequent [state] is returned by the [observer], if the Markov Chain assumption is to hold. *)

  type policy = Policy.t

  val act : policy -> observer -> 'a
  (** [act policy observer] is an infinite (tail-recursive) loop modelling an MDP process. Each iteration of the loop involves 1.) calling the [observer], 2.) infering an [action], the next [observer], and a new policy by calling [Policy.infer] with the current policy and to the observed [state], and 3.) executing the action. *)
end

(** A functor. [Make P] returns an [Actor] module that is parameterised by the policy module [P]. *)
module Make (P : PolicyType) : Actor with module Policy = P
