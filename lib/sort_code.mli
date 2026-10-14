(** UK sort codes: six digits naming a bank branch (or, for most banks today, just the
    bank), written [04-00-04]. With an account number they address a UK account on Faster
    Payments, CHAPS and Bacs. Values are kept as the six digits. *)

open! Core

type t [@@deriving bin_io, sexp, compare, equal, hash, quickcheck]

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

(** Six digits, optionally separated by dashes or spaces ([04-00-04], [04 00 04],
    [040004]). *)
val of_string : string -> t Or_error.t

val of_string_exn : string -> t

(** The six digits, [040004]. *)
val to_string : t -> string

(** The printed form, [04-00-04]. *)
val to_string_hum : t -> string

(** As a number, for range lookups ([040004] is [40004]). *)
val to_int : t -> int
