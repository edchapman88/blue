type response_signal = {
  channel : Unix.sockaddr;
  rolling_window_secs : float;
}

let green_ip () = Cli.green_ip ()
let red_ip () = Cli.red_ip ()

let res_sig () =
  {
    channel = Cli.response_signal_addr ();
    rolling_window_secs = Cli.rolling_window_secs ();
  }

let request_rate () = 1. /. Cli.request_interval ()

let ok_rate =
  (* Maintain a stateful receiver that is initialised at runtime (after the Cli arguments have been parsed). *)
  let receiver = ref None in
  fun () ->
    match !receiver with
    | None ->
        let res_sig = res_sig () in
        let rcv =
          Receiver.(receiver_of_reader @@ reader_of_addr @@ res_sig.channel)
            ~window_secs:res_sig.rolling_window_secs
        in
        receiver := Some rcv;
        rcv ()
    | Some rcv -> rcv ()

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
    | ToggleGreen -> toggle_block @@ green_ip ()
    | ToggleRed -> toggle_block @@ red_ip ()
  in
  match action with
  | Ok _ -> ()
  | Error msg -> failwith msg
