(** Business Identifier Codes (ISO 9362): 4-letter institution, 2-letter country,
    2-character location and an optional 3-character branch. Optional for SEPA payments,
    which route on the IBAN alone. *)

open! Core

type t [@@deriving bin_io, sexp, compare, equal, hash]

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

val of_string : string -> t Or_error.t
val of_string_exn : string -> t
val to_string : t -> string
val country : t -> string

(** Branch code; [XXX] (the primary office) and an 8-character BIC both give [None]. *)
val branch : t -> string option
