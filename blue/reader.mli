type t = unit -> bytes option
(** A lightweight function fetching all bytes that have arrived on some OS interface, e.g. at a serial port, or UDP port. The buffer is emptied in the process. *)

(** [Reader.S] is the interface for modules exposing a [reader_ofaddr] function. The readers returned (of type [Reader.t]) are lightweight (i.e. the connections are initialised once, and maintained with functionally-encapsulated state) and may be called repeatedly in tight loops. *)
module type S = sig
  type addr
  (** The address of the connection to be used by the [Reader.t]. *)

  val reader_of_addr : addr -> t
  (** [reader_of_addr addr] is a [Reader.t] configured to read from the address [addr]. *)
end

module Serial : S with type addr = string
(** Read from serial devices, e.g. "/dev/ttyUSB0". *)

module Udp : S with type addr = Unix.sockaddr
(** Bind to, and read from a UDP socket. *)

