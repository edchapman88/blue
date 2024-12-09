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
end
