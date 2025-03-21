open Domainslib

type t = unit -> float
type arrival = int * float

(* [all_received chan] polls [chan] in a tight loop until there are no messages remaining in the channel buffer, then returns the messages as a [list]. *)
let all_received chan =
  let rec aux acc =
    match Chan.recv_poll chan with
    | None -> List.rev acc
    | Some msg -> aux (msg :: acc)
  in
  aux []

let receiver_of_arrivals ~window_secs msg_chan =
  (* Function-encapsulated state, a list of {e relevant} [arrival]s. [arrival]s with a [timestamp < (now - window_secs)] are no longer relevant for calculating averages over the last [window_secs] seconds, so are dropped from the list.  *)
  let relevant = ref [] in
  fun () ->
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
  let open Reader in
  match addr with
  | Unix.ADDR_UNIX addr -> Serial.reader_of_addr addr
  | Unix.ADDR_INET _ -> Udp.reader_of_addr addr

(* [listen ~reader chan] commences a tight listening loop within which bytes returned by the reader are filtered for 1's, timestamped, and written to the channel [chan] as [arrival]s. *)
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
  receiver_of_arrivals ~window_secs msg_chan
