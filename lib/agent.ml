(** See interface for documentation. *)
module type MarkovCompressorType = sig
  type state
end

(** See interface for documentation. *)
module type RewardType = sig
  type t
  type state

  val fn : state * state -> t option
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

  val infer : t -> state * reward option -> inference
end

(** See interface for documentation. *)
module type Agent = sig
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
    let rec loop policy prev_state observer =
      let state = observer () in
      let reward = Reward.fn (prev_state, state) in
      let Policy.{ action; observer = observer'; policy = policy' } =
        Policy.infer policy (state, reward)
      in
      action ();
      loop policy' state observer'
    in

    (* Start the loop with the [init_observer] defined by the policy module [P]. A reward is undefined until there is a sequence of two states. *)
    let state = Policy.init_observer () in
    let Policy.{ action; observer = observer'; policy = policy' } =
      Policy.infer policy (state, None)
    in
    action ();
    loop policy' state observer'
end
