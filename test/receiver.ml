let print_rate rate_fn () = Printf.printf "%f\n" (rate_fn ())

let%expect_test "" =
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 8081) in
  let rate_fn = Blue.Receiver.receiver 3.0 addr in
  print_rate rate_fn ();
  Unix.sleep 1;
  print_rate rate_fn ();
  [%expect {| |}]
