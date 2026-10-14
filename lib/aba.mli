(** ABA routing transit numbers: nine digits naming a US bank for ACH, RTP and Fedwire.
    The ninth digit is a check digit: [3(d1 + d4 + d7) + 7(d2 + d5 + d8) + (d3 + d6 + d9)]
    is a multiple of 10. A valid checksum does not mean the number is assigned; the
    Federal Reserve's directory says that. *)

open! Core

type t [@@deriving bin_io, sexp, compare, equal, hash, quickcheck]

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

(** Nine digits, spaces and dashes ignored; refuses a wrong check digit. *)
val of_string : string -> t Or_error.t

val of_string_exn : string -> t
val to_string : t -> string

(** The check digit for the first eight digits. *)
val check_digit : string -> int Or_error.t
