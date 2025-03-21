type state = MarkovCompressor.state
type t = int

let string_of_state_reward ((s, reward) : state * t) =
  Printf.sprintf
    "{\"ok_rate\":\"%.2f\",\"green_blocked\":%B,\"red_blocked\":%B,\"reward\":%d}"
    s.ok_rate s.green s.red reward

(** A reward that is a positive leading indicator. [1] if and only if the OK response rate is greater than the acceptable fractionof the perfect/expected value, which is equal to the query/request rate (e.g. negligible number of dropped requests). *)
let fn state =
  let open MarkovCompressor in
  if state.ok_rate > Cli.acceptable_fraction () *. System.request_rate () then 1
  else 0
