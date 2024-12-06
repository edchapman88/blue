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
  type policy

  val act : policy -> 'a
end

(** Implementation for the functor [Make]. *)
module Make (P : PolicyType) = struct
  type policy = P.t

  let act policy =
    (* Define the inner recursive loop. *)
    let rec loop policy observer =
      let obs = observer () in
      let P.{ action; observer = observer'; policy = policy' } =
        P.infer policy obs
      in
      action ();
      loop policy' observer'
    in
    (* Start the loop with the [init_observer] defined by the policy module [P]. *)
    loop policy P.init_observer
end
