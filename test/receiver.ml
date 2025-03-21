open Blue

(* Whether or not to round the 'OK rate' values before printing in the following expect tests. Must be [true] for the tests to pass consistently, [false] may be useful for debugging. *)
let rounded = true

let print_rate rate_fn =
  let rate = if rounded then rate_fn () |> Float.round else rate_fn () in
  Printf.printf "OK rate = %.0f\n%!" rate

(* [mock_sender ~rate chan] sends mock data over the channel [chan] at a constant [rate], indefinitely. *)
let mock_sender ~rate msg_chan =
  let period = 1. /. rate in
  let rec loop () =
    Unix.sleepf period;
    let time = Unix.gettimeofday () in
    (* [(1, time)] simulates 1 OK signal received at [time]. *)
    Domainslib.Chan.send msg_chan (1, time);
    loop ()
  in
  loop ()

(* Spawn a domain in which [mock_sender] will run. The channel [msg_chan] receives mock data at a constant rate and is passed to [Receiver.ok_rate] to return a 'getter' for the average rate. *)
let mock_receiver ~window_secs ~send_rate =
  let msg_chan = Domainslib.Chan.make_unbounded () in
  let _mock_sender =
    Domain.spawn (fun () -> mock_sender ~rate:send_rate msg_chan)
  in
  Blue.Receiver.ok_rate window_secs msg_chan

let%expect_test "test rate evaluation" =
  (* A mock serial receiver evaluating an average OK rate with a look-back period of 4 seconds. The mock data has a constant OK rate of 10 per second. Hence the mock receiver should evaluate an average OK rate of 10. *)
  let rate_getter = mock_receiver ~window_secs:3.0 ~send_rate:10.0 in
  (* Warmup period of 5 seconds (longer than the lookback period of 4 seconds). *)
  Unix.sleep 4;
  print_rate rate_getter;
  Unix.sleep 1;
  print_rate rate_getter;
  [%expect {|
  OK rate = 10
  OK rate = 10
  |}]

(* Mocks a reader (e.g. a serial or UDP reader), returning [bytes option]. Assuming the reader is called in quite a tight loop (with little computation between calls), [mock_reader] will return appoximately 10 OK signals (note: a value of '1' is an OK signal and a value of '0' is an ERR signal) every second: an OK rate of 10. *)
let mock_reader () =
  Unix.sleepf 0.2;
  Some (Bytes.of_string "10010")

let%expect_test "test listening loop" =
  (* Test the (tight) listening loop that is spawned in a sperate OS thread by [Receiver.receiver_of_reader], using [mock_reader]. *)
  let window_secs = 3.0 in
  let rate_getter = Receiver.receiver_of_reader ~window_secs mock_reader in
  Unix.sleep 4;
  print_rate rate_getter;
  Unix.sleep 1;
  print_rate rate_getter;
  [%expect {|
    OK rate = 10
    OK rate = 10
    |}]
