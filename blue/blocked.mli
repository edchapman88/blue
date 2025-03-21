(** Utility functions to query and edit a firewall blocklist managed by {{:https://www.nftables.org/}nftables}. *)

module BlockedSet : Set.S with type elt = String.t
(** A [Set] of blocked IP addresses. *)

val blocked_of_string : string -> BlockedSet.t
(** [blocked_of_string raw_output] is the set of blocked IP addresses parsed from the [raw_output] string that is written to [stdout] by the nftables binary. *)

val query_blocked : unit -> BlockedSet.t
(** Returns the set of IP addresses that are currently in the blocklist managed by nftables. *)

val toggle_block : string -> (unit, string) result
(** [toggle_block ip_addr] is [Ok ()] if the blocked/unblocked status of [ip_addr] was successfully toggled, [Error msg] otherwise; where [msg] notes which [ip_addr] has failed to toggle. *)
