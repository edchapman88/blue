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

let body_of_sr (state, reward) =
  (state, reward) |> Reward.string_of_state_reward |> Cohttp_lwt.Body.of_string

let try_promise promise =
  Lwt.try_bind
    (fun () -> promise)
    (fun res -> Lwt.return (Ok res))
    (fun e -> Lwt.return (Error (Printexc.to_string e)))

let request (state, reward) =
  let body = body_of_sr (state, reward) in
  Cohttp_lwt_unix.Client.post ~body
    (Uri.of_string @@ Option.get @@ Cli.server_policy ())
  |> try_promise

let post (state, reward) =
  let open Lwt.Infix in
  let res_body = request (state, reward) >|= Result.map snd in
  let body_string =
    res_body
    >>= (function
          | Ok body -> try_promise @@ Cohttp_lwt.Body.to_string body
          | Error msg -> Lwt.return (Error msg))
    |> Lwt_main.run
  in
  body_string |> fun r ->
  Result.bind r System.eff_of_string
  |> Result.map_error @@ fun reason -> "PolicyServer failed with: " ^ reason

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
