(** When a standing order runs. Dates only: the order is due from the start of its run
    date in the caller's time zone. *)

open! Core

type t =
  | Daily
  | Weekly of Day_of_week.Stable.V1.t
  | Monthly of { day : int (** 1 to 31; clamped to the last day of shorter months *) }
[@@deriving bin_io, sexp, compare, equal, hash]

(** Fails on a monthly day outside 1..31. *)
val validate : t -> unit Or_error.t

(** The first run date on or after [date]. *)
val next_on_or_after : t -> Date.t -> Date.t

(** The first run date strictly after [date]. *)
val next_after : t -> Date.t -> Date.t
