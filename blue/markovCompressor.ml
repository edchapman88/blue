type state = {
  ok_rate : float;
  green : bool;
  red : bool;
}
(** Rate of OK responses to the web-client and blocked status for the IP addresses of the two other hosts in the network. The web-client, [green], and the adversary, [red]. *)

let observe () =
  let is_blocked ip =
    let open System in
    BlockedSet.mem ip (blocked_ips ())
  in
  {
    ok_rate = System.ok_rate ();
    green = is_blocked @@ System.green_ip ();
    red = is_blocked @@ System.red_ip ();
  }
