(* Blue Executable: Defending a web-server.

   Commence a [Markov.Agent.S.act] loop to make observations of, and take actions in, the cyber system.

   A module of type [Markov.Agent.S] is created using the functor [Markov.Agent.Make]. The functor has three arguments which must be defined first:

   1. A [Markov.Agent.MarkovCompressorType] module.
   2. A [Markov.Agent.RewardType] module.
   3. A [Markov.Agent.RLPolicyType] module.
*)

module System = struct
  let green_ip = "172.0.0.2"
  let red_ip = "172.0.0.3"
  let request_rate = 20

  module BlockedSet = Set.Make (String)

  let ok_rate () = failwith "TODO"
  let blocked_ips () = BlockedSet.empty

  type eff =
    | Wait
    | ToggleGreen
    | ToggleRed

  let random_eff () =
    match Random.int 3 with
    | 0 -> Wait
    | 1 -> ToggleGreen
    | 2 -> ToggleRed
    | _ -> failwith "unreachable"

  let exec_eff eff =
    match eff with
    | Wait -> ()
    | ToggleGreen -> failwith "TODO"
    | ToggleRed -> failwith "TODO"
end

module MarkovCompressor = struct
  type state = {
    ok_rate : int;
    green : bool;
    red : bool;
  }
  (** Rate of OK responses to the web-client and blocked status for the IP addresses of the two other hosts in the network. The web-client, [green], and the adversary, [red]. *)

  let observe () =
    let is_blocked ip =
      let open System in
      BlockedSet.exists (String.equal ip) (blocked_ips ())
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

  (** A reward that is a positive leading indicator. [1] if and only if the OK response rate is at 20 Responses Per Second (RPS), which is equal to the query/request rate (e.g. negligible number of dropped requests). *)
  let fn state =
    let open MarkovCompressor in
    if state.ok_rate = System.request_rate then 1 else 0
end

module Policy = struct
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

  type t = int RewardTable.t
  (** [Policy.t] is a table of cumulative historic reward for each of the 4 possible configurations of [config]. *)

  let init () = RewardTable.empty
  let init_observer = MarkovCompressor.observe

  type inference = {
    action : action;
    observer : observer;
    policy : t;
  }

  (** [push (config,reward) policy] pushes the [(config, reward)] instance onto the table of cumulative historic rewards. *)
  let push (config, reward) table =
    RewardTable.update config
      (fun prev ->
        match prev with
        | None -> Some reward
        | Some sum -> Some (sum + reward))
      table

  let most_valuable policy =
    RewardTable.fold
      (fun k v (k_max, v_max) -> if v > v_max then (k, v) else (k_max, v_max))
      policy
      ({ green = true; red = true }, Int.min_int)

  let infer policy (state, reward) =
    let config = config_of_state state in
    let policy' = push (config, reward) policy in
    let goal, _ = most_valuable policy in
    let chosen_eff =
      (* Exploration. *)
      if RewardTable.cardinal policy < 4 then System.random_eff ()
      else if (* Exploitation. *)
              config = goal then System.(Wait)
      else if config.green = goal.green then System.(ToggleRed)
      else System.(ToggleGreen)
    in
    let action () = System.exec_eff chosen_eff in
    {
      action;
      observer =
        (fun () ->
          Unix.sleep 5;
          MarkovCompressor.observe ());
      policy = policy';
    }
end

module Defender = Markov.Agent.Make (MarkovCompressor) (Reward) (Policy)

let () =
  Random.init 0;
  Defender.act (Policy.init ())
