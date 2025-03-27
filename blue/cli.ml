let usage_msg =
  "\n\
  \ Usage:\n\
  \ blue [-l <log-dir>] [-e <n-exploration-steps>] [-t <obs-time-delay>] [-f \
   <acceptable-fraction>] [-i <request-interval>] [-g <green-ip>] [-r \
   <red-ip>]  [-a <response-signal-addr>] [-s <rolling-window-secs>] \n\n\
  \ Example:\n\
  \ blue -l /home/ed/blue_logs -i 0.05 -g 172.0.0.2 -r 172.0.0.3 -a \
   /dev/ttyUSB0 -s 3.0 \n\n\
  \ Options:"

let parse_addr raw_addr =
  if String.contains raw_addr ':' then
    try
      match String.split_on_char ':' raw_addr with
      | [ ip; port ] ->
          Unix.ADDR_INET (Unix.inet_addr_of_string ip, int_of_string port)
      | _ -> raise @@ Failure "Network addresses must have exactly one ':'."
    with Failure msg ->
      failwith
      @@ Printf.sprintf
           "Failed parsing response signal address: '%s'. Accepted formats are \
            e.g. '/dev/ttyUSB0' or '172.0.1.1:8081'. %s"
           raw_addr msg
  else Unix.ADDR_UNIX raw_addr

let _log_path = ref ""
let _obs_time_delay = ref 5.0
let _acceptable_fraction = ref 0.8
let _request_interval = ref 1.
let _green_ip = ref "172.0.0.2"
let _red_ip = ref "172.0.0.3"
let _response_signal_addr = ref "/dev/ttyUSB0"
let _parsed_response_signal_addr = ref (parse_addr !_response_signal_addr)
let _rolling_window_secs = ref 3.0
let _n_exploration_steps = ref 300
let _server_policy_addr = ref ""

let speclist =
  [
    ( "-l",
      Arg.Set_string _log_path,
      ": Optionally write a log file in the specified directory with \
       information about the sequence of states observed and actions taken. If \
       no log file is specified, the information is written to stdout.\n" );
    ( "-t",
      Arg.Set_float _obs_time_delay,
      ": Set the constant time delay between observations in seconds. Defaults \
       to 5.0 seconds.\n" );
    ( "-f",
      Arg.Set_float _acceptable_fraction,
      ": Set the acceptable fraction of OK responses for a positive reward. \
       E.g. for a request rate of 20 RPS (Requests Per Second), give a \
       positive reward for response rates > (0.8 * 20), where 0.8 is the \
       acceptable fraction. Defaults to '0.8'.\n" );
    ( "-i",
      Arg.Set_float _request_interval,
      ": Set the client request interval (delay between requests) in seconds. \
       Compared with the OK response rate reported by the client to determine \
       the reward. Defaults to 1.0.\n" );
    ( "-g",
      Arg.Set_string _green_ip,
      ": Set the IP address of the client (the green host), defaults to \
       '172.0.0.2'.\n" );
    ( "-r",
      Arg.Set_string _red_ip,
      ": Set the IP address of the adversary (the red host), defaults to \
       '172.0.0.3'.\n" );
    ( "-a",
      Arg.Set_string _response_signal_addr,
      ": Set the address of the out-of-band channel used by the client over \
       which 1's and 0's are sent to indicate successful and failed responses \
       received by the client. Defaults to '/dev/ttyUSB0'. If a filesystem \
       address, a serial reader is initialised; if a network address in the \
       form '172.0.1.3:8081', a UDP reader is initialised. In the UDP case, \
       the IP address should be the IP *used by the sender*.\n" );
    ( "-s",
      Arg.Set_float _rolling_window_secs,
      ": Set the length of the rolling window used to evaluate the average OK \
       response rate indicated by the data received over the out-of-band \
       channel with the client. Defaults to 3.0.\n" );
    ( "-e",
      Arg.Set_int _n_exploration_steps,
      ": Set the number of observations used by the policy for exploration, \
       after which actions are selected for exploitation. Defaults to 300.\n" );
    ( "--server-policy",
      Arg.Set_string _server_policy_addr,
      ": Set the IP address of a policy server, overriding the \
       CountBasedPolicy. Serialised observations and rewards are sent as POST \
       requests to the configured address and actions are parsed from the \
       responses. The '-e' flag, used to configure the CountBasedPolicy, is \
       ignored when '--server-policy' is used.\n" );
  ]

let log_path () =
  if String.length !_log_path == 0 then None else Some !_log_path

let server_policy () =
  if String.length !_server_policy_addr == 0 then None
  else Some !_server_policy_addr

let n_exploration_steps () = !_n_exploration_steps
let obs_time_delay () = !_obs_time_delay
let acceptable_fraction () = !_acceptable_fraction
let request_interval () = !_request_interval
let green_ip () = !_green_ip
let red_ip () = !_red_ip
let response_signal_addr () = !_parsed_response_signal_addr
let rolling_window_secs () = !_rolling_window_secs

let arg_parse () =
  let parse_positional_args _ = () in
  Arg.parse speclist parse_positional_args usage_msg;
  let raw_addr = !_response_signal_addr in
  _parsed_response_signal_addr := parse_addr raw_addr
