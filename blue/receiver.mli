(** Set up serial and UDP listeners on OS interfaces (e.g. serial ports or UDP ports) to consume byte (not bit!) streams of 1's and 0's. Create 'receivers' (of type [Receiver.t]), which are functions returning up-to-date estimates for the average rate of 1's (the 'OK' rate) received. *)

type t = unit -> float
(** [Receiver.t] is a function that returns an up-to-date estimate for the average rate of 1's received over an OS information channel, e.g. a serial port or UDP port. The average is calculated over an n second rolling window, where n is configured in the construction of the [Receiver.t]. *)

type arrival = int * float
(** [(n_at_t, t)], is an [arrival] of n things at time [t], where [t] is a Unix timestamp (in seconds). *)

val receiver_of_arrivals : window_secs:float -> arrival Domainslib.Chan.t -> t
(** [receiver_of_arrivals ~window_secs chan] is a [Receiver.t] that, when queried, returns the average rate of arrivals received over the channel [chan]. The average is calculated over the last [window_secs] seconds. *)

val reader_of_addr : Unix.sockaddr -> Reader.t
(** [reader_of_addr addr] returns a lightweight function that can be called repeatedly to fetch data arriving at the OS interface at the address [addr], e.g. a serial port or UDP port. Only serial or UDP readers are supported. If [addr] is of type [Unix.ADDR_INET], a UDP reader is initialised; if [Unix.ADDR_UNIX], a serial reader. *)

val receiver_of_reader : window_secs:float -> Reader.t -> t
(** [receiver_of_reader ~window_secs reader] is a [Receiver.t] that, when queried, returns the average rate of 1's (the 'OK' rate) consumed by the [Reader.S.t] (the reader will be attached to some OS interface, e.g a serial port or UDP port). *)
