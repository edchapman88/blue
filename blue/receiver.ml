open Domainslib

let serial addr =
  print_endline "serial fn";
  match addr with
  | Unix.ADDR_INET _ -> failwith "unreachable"
  | Unix.ADDR_UNIX dev -> Serial.read dev

let udp addr =
  let fd = Unix.socket ~cloexec:false PF_INET SOCK_DGRAM 0 in
  Unix.bind fd addr;
  (*Unix.listen fd 1000;*)
  let buf_len = 1024 in
  let buf = Bytes.create buf_len in
  let len, _src = Unix.recvfrom fd buf buf_len 0 [] in
  Unix.close fd;
  if len = 0 then None else Some (Bytes.sub buf 0 len)

let all_received chan =
  let rec aux acc =
    match Chan.recv_poll chan with
    | None -> List.rev acc
    | Some msg -> aux (msg :: acc)
  in
  aux []

let relevant = ref []

let ok_rate window_secs msg_chan () =
  let buf = !relevant @ all_received msg_chan in
  let relevant', num_ok =
    List.fold_left
      (fun (rel, count) (n_at_t, t) ->
        if t < Unix.gettimeofday () -. window_secs then (rel, count)
        else ((n_at_t, t) :: rel, count + n_at_t))
      ([], 0) buf
  in
  relevant := List.rev relevant';
  float_of_int num_ok /. window_secs

let listener_of_addr addr =
  match addr with
  | Unix.ADDR_UNIX _ -> serial
  | Unix.ADDR_INET _ -> udp

let listen ~listener chan =
  let rec loop () =
    (*Unix.sleepf 0.0001;*)
    (*let time = Unix.gettimeofday () in*)
    (*if time -. Float.floor time < 0.001 then Printf.printf "time: %f\n" time;*)
    match listener () with
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

let receiver_of_listener ~window_secs listener =
  let msg_chan = Chan.make_unbounded () in
  let _listener = Domain.spawn (fun () -> listen ~listener msg_chan) in
  (*Chan.send msg_chan (Unix.gettimeofday ());*)
  ok_rate window_secs msg_chan
