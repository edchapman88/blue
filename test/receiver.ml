let print_rate rate_fn = Printf.printf "OK rate = %.0f\n" (rate_fn ())

let mock_send ~rate msg_chan =
  let period = 1. /. rate in
  let rec loop () =
    Unix.sleepf period;
    let time = Unix.gettimeofday () in
    Domainslib.Chan.send msg_chan time;
    loop ()
  in
  loop ()

let mock_receiver ~window_secs ~send_rate =
  let msg_chan = Domainslib.Chan.make_unbounded () in
  let _mock_sender =
    Domain.spawn (fun () -> mock_send ~rate:send_rate msg_chan)
  in
  Blue.Receiver.ok_rate window_secs msg_chan

let%expect_test "mock incoming serial data" =
  (* A mock serial receiver evaluating an average OK rate with a look-back period of 4 seconds. The mock data has a constant OK rate of 10 per second. Hence the mock receiver should evaluate an average OK rate of 10. *)
  let rate_getter = mock_receiver ~window_secs:4.0 ~send_rate:10.0 in
  (* Warmup period of 5 seconds (longer than the lookback period of 4 seconds). *)
  Unix.sleep 5;
  print_rate rate_getter;
  Unix.sleep 1;
  print_rate rate_getter;
  [%expect {|
    OK rate = 10
    OK rate = 10
    |}]

(*let%expect_test "udp" =*)
(*let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 8081) in*)
(*let rate_fn = Blue.Receiver.receiver 3.0 addr in*)
(*print_rate rate_fn;*)
(*Unix.sleep 1;*)
(*print_rate rate_fn;*)
(*[%expect {| |}]*)

(*let%expect_test "serial" =*)
(*let addr = Unix.ADDR_UNIX "/dev/tty.usbserial-FTCGF6A9" in*)
(*let rate_fn = Blue.Receiver.receiver 3.0 addr in*)
(*Unix.sleep 1;*)
(*print_rate rate_fn;*)
(*Unix.sleep 1;*)
(*print_rate rate_fn;*)
(*[%expect {| |}]*)

(*let%expect_test "single udp" =*)
(*let addr = Unix.ADDR_INET (Unix.inet_addr_any, 8081) in*)
(*(match Blue.Receiver.udp addr () with*)
(*| None -> print_endline "none"*)
(*| Some _ -> print_endline "some");*)
(*[%expect {| |}]*)
