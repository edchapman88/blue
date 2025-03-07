(* Blue Executable: Defending a web-server.

   Commence a [Markov.Agent.S.act] loop to make observations of, and take actions in, the cyber system.

   A module of type [Markov.Agent.S] is created using the functor [Markov.Agent.Make]. The functor has three arguments which must be defined first:

   1. A [Markov.Agent.MarkovCompressorType] module.
   2. A [Markov.Agent.RewardType] module.
   3. A [Markov.Agent.RLPolicyType] module.
*)

let () = Blue.Cli.arg_parse ()

module System = struct
  open Blue

  type response_signal = {
    channel : Unix.sockaddr;
    rolling_window_secs : float;
  }

  let res_sig =
    {
      channel = Cli.response_signal_addr ();
      rolling_window_secs = Cli.rolling_window_secs ();
    }

  let green_ip = Cli.green_ip ()
  let red_ip = Cli.red_ip ()
  let request_rate = 1. /. Cli.request_interval ()

  let ok_rate =
    Receiver.(receiver_of_reader @@ reader_of_addr @@ res_sig.channel)
      ~window_secs:res_sig.rolling_window_secs

  module BlockedSet = Blocked.BlockedSet

  let blocked_ips = Blocked.query_blocked

  type eff =
    | Wait
    | ToggleGreen
    | ToggleRed

  let string_of_eff e =
    match e with
    | Wait -> "Wait"
    | ToggleGreen -> "ToggleGreen"
    | ToggleRed -> "ToggleRed"

  let random_eff () =
    match Random.int 3 with
    | 0 -> Wait
    | 1 -> ToggleGreen
    | 2 -> ToggleRed
    | _ -> failwith "unreachable"

  let exec_eff eff =
    let open Blocked in
    let action =
      match eff with
      | Wait -> Ok ()
      | ToggleGreen -> toggle_block green_ip
      | ToggleRed -> toggle_block red_ip
    in
    match action with
    | Ok _ -> ()
    | Error msg -> failwith msg
end

module MarkovCompressor = struct
  type state = {
    ok_rate : float;
    green : bool;
    red : bool;
  }
  (** Rate of OK responses to the web-client and blocked status for the IP addresses of the two other hosts in the network. The web-client, [green], and the adversary, [red]. *)

  let observe () =
    let is_blocked ip =
      let open System in
      BlockedSet.mem ip (blocked_ips ())
    in
    {
      ok_rate = System.ok_rate ();
      green = is_blocked System.green_ip;
      red = is_blocked System.red_ip;
    }
end

module Reward = struct
  type state = MarkovCompressor.state
  type t = int

  let string_of_state_reward ((s, reward) : state * t) =
    Printf.sprintf
      "{\"ok_rate\":\"%.2f\",\"green_blocked\":%B,\"red_blocked\":%B,\"reward\":%d}"
      s.ok_rate s.green s.red reward

  (** A reward that is a positive leading indicator. [1] if and only if the OK response rate is greater than the acceptable fractionof the perfect/expected value, which is equal to the query/request rate (e.g. negligible number of dropped requests). *)
  let fn state =
    let open MarkovCompressor in
    if state.ok_rate > Blue.Cli.acceptable_fraction () *. System.request_rate
    then 1
    else 0
end

module Policy = struct
  open Blue

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
end

module Defender = Markov.Agent.Make (MarkovCompressor) (Reward) (Policy)

let () =
  Random.init 0;
  Defender.act (Policy.init ())
