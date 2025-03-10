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

let fd_of_dev device =
  let config = { baud = 115200; port = device } in
  let fd = Unix.openfile config.port [ Unix.O_RDONLY; O_NONBLOCK ] 0o000 in
  set_baud fd config.baud;
  fd

let init device =
  let fd = fd_of_dev device in
  serial_conn := Some (Unix.in_channel_of_descr fd)

let read device =
  if Option.is_none !serial_conn then init device;
  let chan = Option.get !serial_conn in
  let buf = Bytes.create 1024 in
  let n_read =
    try In_channel.input chan buf 0 1023 with Sys_blocked_io -> 0
  in
  if n_read = 0 then None else Some (Bytes.sub buf 0 n_read)
