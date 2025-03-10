(** A stateful module, initialised with [init ()]. The serial port address is configured (optionally at runtime) in the [Cli] module. A serial connection of type [t] is maintained as mutable state and written to by calls to [write_line] or [write_score]. **)

type t
(** A serial connection. *)

type config = {
  baud : int;  (** Baude rate. *)
  port : string;
      (** File system address of the serial device, e.g. "/dev/tty0". *)
}
(** Configuration for a connection of type [Serial.t]. *)

val init : string -> unit
(** [init port] initialises the module, using the serial port address [port]. *)

val read : string -> bytes option
