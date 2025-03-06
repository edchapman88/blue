type config = {
  baud : int;
  port : string;
}

type t = In_channel.t

(** Set the baud rate on a Unix file *)
let set_baud fd rate =
  fd |> Unix.tcgetattr |> fun attr ->
  Unix.tcsetattr fd Unix.TCSANOW
    {
      attr with
      c_ibaud = rate;
      c_obaud = rate;
      c_echo = false;
      c_icanon = false;
      c_vtime = 5;
      c_clocal = true;
    }

let serial_conn = ref None

let init device =
  let config = { baud = 115200; port = device } in
  let fd = Unix.openfile config.port [ Unix.O_RDWR; Unix.O_NONBLOCK ] 0o000 in
  set_baud fd config.baud;
  let chan = Unix.in_channel_of_descr fd in
  serial_conn := Some chan

let read device =
  if Option.is_none !serial_conn then init device;
  let chan = Option.get !serial_conn in
  let buf = Bytes.create 1024 in
  let n_read = In_channel.input chan buf 0 1023 in
  if n_read = 0 then None else Some (Bytes.sub buf 0 n_read)
