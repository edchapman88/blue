type t = unit -> bytes option

module type S = sig
  type addr

  val reader_of_addr : addr -> t
end

module Serial : S with type addr = string = struct
  type addr = string

  type config = {
    baud : int;
    port : string;
  }

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

  let fd_of_dev device =
    let config = { baud = 115200; port = device } in
    let fd = Unix.openfile config.port [ Unix.O_RDONLY; O_NONBLOCK ] 0o000 in
    set_baud fd config.baud;
    fd

  let conn_of_dev device =
    let fd = fd_of_dev device in
    Unix.in_channel_of_descr fd

  let read connection =
    let buf = Bytes.create 1024 in
    let n_read =
      try In_channel.input connection buf 0 1023 with Sys_blocked_io -> 0
    in
    if n_read = 0 then None else Some (Bytes.sub buf 0 n_read)

  let reader_of_addr device =
    let conn = ref None in
    fun () ->
      if Option.is_none !conn then conn := Some (conn_of_dev device);
      read @@ Option.get !conn
end

module Udp : S with type addr = Unix.sockaddr = struct
  type addr = Unix.sockaddr

  let sock_of_addr addr =
    let fd = Unix.socket PF_INET SOCK_DGRAM 0 in
    Unix.bind fd addr;
    fd

  let read sock =
    let buf_len = 1 in
    let buf = Bytes.create buf_len in
    let len, _src = Unix.recvfrom sock buf 0 buf_len [] in
    if len = 0 then None else Some buf

  let reader_of_addr addr =
    let sock = ref None in
    fun () ->
      if Option.is_none !sock then sock := Some (sock_of_addr addr);
      read @@ Option.get !sock
end
