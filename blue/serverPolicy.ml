type reward = Reward.t
type state = MarkovCompressor.state
type observer = unit -> state
type action = unit -> unit

type t = int
(** Number of policy steps. *)

type inference = {
  action : action;
  observer : observer;
  policy : t;
}

let init () = 0
let init_observer = MarkovCompressor.observe
let time_before_retry = 3

let request (state, reward) =
  let addr = Cli.server_policy () |> Option.get in
  let sock = Unix.(socket PF_INET SOCK_STREAM 0) in
  let _ = Unix.connect sock addr in

  let in_ch = Unix.in_channel_of_descr sock in
  let out_ch = Unix.out_channel_of_descr sock in
  let body = (state, reward) |> Reward.string_of_state_reward in
  output_string out_ch
  @@ Printf.sprintf
       "POST / HTTP/1.1\r\n\
        Accept: application/json\r\n\
        Content-Type: application/json\r\n\
        Content-Length: %d\r\n\
        \r\n\
        %s"
       (String.length body) body;
  flush out_ch;

  let rec readall ic acc =
    try readall ic (acc ^ input_line ic)
    with End_of_file ->
      Unix.close sock;
      acc
  in

  let res = readall in_ch "" in
  let start_bod = String.rindex_from res (String.length res - 1) '\r' + 1 in
  let len_bod = String.length res - start_bod in
  String.sub res start_bod len_bod

let post (state, reward) = request (state, reward) |> System.eff_of_string

let infer policy (state, reward) =
  (* Log state and reward. *)
  Log.write_msg @@ Reward.string_of_state_reward (state, reward);

  (* Send [state] and [reward] to policy server. *)
  let chosen_eff =
    match post (state, reward) with
    | Error msg -> (
        (* Write policy server failure to log and do one retry. *)
        Log.write_msg msg;
        Unix.sleep time_before_retry;
        (* 'Fail-fast' if the policy server fails after one retry. *)
        match post (state, reward) with
        | Error msg ->
            let msg = "On first (and only) retry: " ^ msg in
            Log.write_msg msg;
            failwith msg
        | Ok eff -> eff)
    | Ok eff -> eff
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
    policy = policy + 1;
  }
