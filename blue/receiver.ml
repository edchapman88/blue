open Domainslib

let recvfrom addr =
  let fd = Unix.socket ~cloexec:true PF_INET SOCK_DGRAM 0 in
  Unix.bind fd addr;
  (*Unix.listen fd 100;*)
  let buf_len = 100 in
  let buf = Bytes.create buf_len in
  let len, _src = Unix.recvfrom fd buf buf_len 0 [] in
  (*Unix.close fd;*)
  if len = 0 then None
  else (
    print_endline "got";
    Some (Bytes.sub buf 0 len))

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
      (fun (rel, n) t ->
        if t < Unix.gettimeofday () -. window_secs then (rel, n)
        else (t :: rel, n + 1))
      ([], 0) buf
  in
  relevant := List.rev relevant';
  float_of_int num_ok /. window_secs

let rec listen chan addr =
  match recvfrom addr with
  | None -> listen chan addr
  | Some msgs ->
      Bytes.iter
        (fun msg ->
          if msg = '1' then
            let time = Unix.gettimeofday () in
            Chan.send chan time)
        msgs;
      listen chan addr

let receiver window_secs addr =
  let msg_chan = Chan.make_unbounded () in
  let _listener = Domain.spawn (fun () -> listen msg_chan addr) in
  ok_rate window_secs msg_chan
