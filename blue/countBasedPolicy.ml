type reward = Reward.t
type state = MarkovCompressor.state
type observer = unit -> state
type action = unit -> unit

type config = {
  green : bool;
  red : bool;
}
(** The configurable part of the [state] space. *)

let config_of_state (state : MarkovCompressor.state) =
  { green = state.green; red = state.red }

module RewardTable = Map.Make (struct
  type t = config

  let compare a b =
    let first_slot = Bool.compare a.green b.green in
    if first_slot != 0 then first_slot else Bool.compare a.red b.red
end)

type t = {
  hist_reward : int RewardTable.t;
  n_steps : int;
}
(** [Policy.t] includes a table of cumulative historic reward for each of the 4 possible configurations of [config], and record of the total number of observations seen by the policy ([n_steps], used determine when exploitation should be favoured over exploration). *)

let init () = { hist_reward = RewardTable.empty; n_steps = 0 }
let init_observer = MarkovCompressor.observe

type inference = {
  action : action;
  observer : observer;
  policy : t;
}

(** [push (config,reward) policy] pushes the [(config, reward)] instance onto the table of cumulative historic rewards, and increments the counter for the total number of observations seen by the policy. *)
let push (config, reward) { hist_reward; n_steps } =
  let hist_reward' =
    RewardTable.update config
      (fun prev ->
        match prev with
        | None -> Some reward
        | Some sum -> Some (sum + reward))
      hist_reward
  in
  { hist_reward = hist_reward'; n_steps = n_steps + 1 }

let most_valuable policy =
  RewardTable.fold
    (fun k v (k_max, v_max) -> if v > v_max then (k, v) else (k_max, v_max))
    policy.hist_reward
    ({ green = true; red = true }, Int.min_int)

let infer policy (state, reward) =
  (* Log state and reward. *)
  Log.write_msg @@ Reward.string_of_state_reward (state, reward);

  let config = config_of_state state in
  let policy' = push (config, reward) policy in
  let goal, _ = most_valuable policy in
  let chosen_eff =
    (* Exploration. *)
    if policy'.n_steps < Cli.n_exploration_steps () then System.random_eff ()
    else if (* Exploitation. *)
            config = goal then System.(Wait)
    else if config.green = goal.green then System.(ToggleRed)
    else System.(ToggleGreen)
  in

  (* Log chosen effect. *)
  Log.write_msg @@ System.string_of_eff chosen_eff;

  let action () = System.exec_eff chosen_eff in
  {
    action;
    observer =
      (fun () ->
        Unix.sleepf @@ Cli.obs_time_delay ();
        MarkovCompressor.observe ());
    policy = policy';
  }
