open Domainslib

let serial addr () =
  match addr with
  | Unix.ADDR_INET _ -> failwith "unreachable"
  | Unix.ADDR_UNIX dev -> Serial.read dev

let udp_sock = ref None

let init_udp addr =
  let fd = Unix.socket PF_INET SOCK_DGRAM 0 in
  Unix.bind fd addr;
  (match addr with
  | Unix.ADDR_INET (ip, port) ->
      Printf.printf "bound to %s, port %d" (Unix.string_of_inet_addr ip) port
  | _ -> print_endline "didn't bind to INET addr!");
  udp_sock := Some fd

let udp addr () =
  if Option.is_none !udp_sock then init_udp addr;
  let sock = Option.get !udp_sock in
  let buf_len = 1 in
  let buf = Bytes.create buf_len in
  let len, _src =
    try Unix.recvfrom sock buf 0 buf_len []
    with _e ->
      print_endline "unhandled exception!";
      (0, addr)
  in
  if len = 0 then None else Some buf

let all_received chan =
  let rec aux acc =
    match Chan.recv_poll chan with
    | None -> List.rev acc
    | Some msg -> aux (msg :: acc)
  in
  aux []

let relevant = ref []

let ok_rate window_secs msg_chan () =
  let start_win = Unix.gettimeofday () -. window_secs in
  let buf = !relevant @ all_received msg_chan in
  let relevant', num_ok =
    List.fold_left
      (fun (rel, count) (n_at_t, t) ->
        if t < start_win then (rel, count)
        else ((n_at_t, t) :: rel, count + n_at_t))
      ([], 0) buf
  in
  relevant := List.rev relevant';
  float_of_int num_ok /. window_secs

let reader_of_addr addr =
  match addr with
  | Unix.ADDR_UNIX _ -> serial addr
  | Unix.ADDR_INET _ -> udp addr

let listen ~reader chan =
  let rec loop () =
    match reader () with
    | None -> loop ()
    | Some msgs ->
        let time = Unix.gettimeofday () in
        let n_ok =
          Bytes.fold_left
            (fun acc char -> if char = '1' then acc + 1 else acc)
            0 msgs
        in
        Chan.send chan (n_ok, time);
        loop ()
  in
  loop ()

let receiver_of_reader ~window_secs reader =
  let msg_chan = Chan.make_unbounded () in
  let _listener = Domain.spawn (fun () -> listen ~reader msg_chan) in
  ok_rate window_secs msg_chan
