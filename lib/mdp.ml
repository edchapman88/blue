(** The input signature of the functor [Make]. *)
module type PolicyType = sig
  type t
  type state
  type observer = unit -> state
  type action = unit -> unit

  type inference = {
    action : action;
    observer : observer;
  }

  val infer : t -> state -> inference
end

(** The output signature of the functor [Make]. *)
module type S = sig
  type state
  type observer = unit -> state
  type action = unit -> unit
  type policy

  val act : policy -> action -> observer -> 'a
end

module Make (Policy : PolicyType) : S = struct
  type state = Policy.state
  type observer = Policy.observer
  type action = Policy.action
  type policy = Policy.t

  let rec act policy action observer =
    let open Policy in
    action ();
    let obs = observer () in
    let { action = action'; observer = observer' } = Policy.infer policy obs in
    act policy action' observer'
end
