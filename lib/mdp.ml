(** See interface for documentation. *)
module type PolicyType = sig
  type t

  val init : unit -> t

  type state
  type observer = unit -> state

  val init_observer : observer

  type action = unit -> unit

  type inference = {
    action : action;
    observer : observer;
    policy : t;
  }

  val infer : t -> state -> inference
end

(** See interface for documentation. *)
module type Actor = sig
  module Policy : PolicyType

  type state = Policy.state
  type observer = Policy.observer
  type action = Policy.action
  type policy = Policy.t

  val act : policy -> observer -> 'a
end

(** Implementation for the functor [Make]. *)
module Make (P : PolicyType) = struct
  module Policy = P

  type state = Policy.state
  type observer = Policy.observer
  type action = Policy.action
  type policy = Policy.t

  let rec act policy observer =
    let obs = observer () in
    let Policy.{ action; observer = observer'; policy = policy' } =
      Policy.infer policy obs
    in
    action ();
    act policy' observer'
end
