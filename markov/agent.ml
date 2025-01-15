(** See interface for documentation. *)
module type MarkovCompressorType = sig
  type state

  val observe : unit -> state
end

(** See interface for documentation. *)
module type RewardType = sig
  type t
  type state

  val fn : state -> t
end

(** See interface for documentation. *)
module type RLPolicyType = sig
  type t

  val init : unit -> t

  type state
  type reward
  type observer = unit -> state

  val init_observer : observer

  type action = unit -> unit

  type inference = {
    action : action;
    observer : observer;
    policy : t;
  }

  val infer : t -> state * reward -> inference
end

(** See interface for documentation. *)
module type S = sig
  type policy

  val act : policy -> 'a
end

(** Implementation for the functor [Make]. *)
module Make
    (MarkovCompressor : MarkovCompressorType)
    (Reward : RewardType with type state = MarkovCompressor.state)
    (Policy : RLPolicyType
                with type state = MarkovCompressor.state
                with type reward = Reward.t) =
struct
  type policy = Policy.t

  let act policy =
    (* Define the inner recursive loop. *)
    let rec loop policy observer =
      let state = observer () in
      let reward = Reward.fn state in
      let Policy.{ action; observer = observer'; policy = policy' } =
        Policy.infer policy (state, reward)
      in
      action ();
      loop policy' observer'
    in

    (* Start the loop with the [init_observer] defined by the policy module [P]. *)
    loop policy Policy.init_observer
end
