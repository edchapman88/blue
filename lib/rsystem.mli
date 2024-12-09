(** A reward-managed system. This stateful module buffers the rewards that are dispensed by the underlying system until an observation is made, at which point the aggregated reward (since the previous observation) is calculated and returned as part of the [observation]. *)
module type RsystemType = sig
  type state
  type reward
  type reward_buf
  type aggr_reward

  val aggregator : reward_buf -> aggr_reward

  type observation = {
    state : state;
    aggr_reward : aggr_reward;
  }

  val observer : unit -> observation
  (** [observer ()] is an [observation] where the contained [reward] is the cumulative reward dispensed by the system {i since the previous call to [observer]}. *)
end
