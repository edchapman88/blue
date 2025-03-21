open Blue.Blocked

(* Example output from the Linux [nft] firewall program when the members of the 'blocked' set are queried. Must be parsed for the IP addresses. *)
let raw_blocked =
  " \n\
   table inet filter {\n\
   \tset blocked {\n\
   \t\ttype ipv4_addr\n\
   \t\telements = { 192.168.0.1, 192.168.0.2 }\n\
   \t}\n\
   }"

let%expect_test "parse nft output for [BlockedSet] of IP addresses" =
  let blocked_set = blocked_of_string raw_blocked in
  BlockedSet.iter print_endline blocked_set;
  [%expect {|
    192.168.0.1
    192.168.0.2
    |}]
