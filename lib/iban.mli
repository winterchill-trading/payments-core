(** International Bank Account Numbers (ISO 13616), validated with the per-country length
    and the mod-97 check digits. Values are kept in electronic form: upper case, no
    spaces. *)

open! Core

type t [@@deriving bin_io, sexp, compare, equal, hash, quickcheck]

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

(** Accepts spaces and lower case (as printed on statements); rejects unknown countries,
    wrong lengths, non-alphanumeric characters and bad check digits. *)
val of_string : string -> t Or_error.t

val of_string_exn : string -> t

(** Electronic form, e.g. [DE89370400440532013000]. *)
val to_string : t -> string

(** Print form in groups of four, e.g. [DE89 3704 0044 0532 0130 00]. *)
val to_string_grouped : t -> string

(** ISO 3166 alpha-2 country code, the first two characters. *)
val country : t -> string

val bban : t -> string

(** The country belongs to the SEPA scheme, so SEPA rails can reach the account. *)
val is_sepa : t -> bool

(** IBAN length for a country, [None] when the country does not issue IBANs (or is not in
    the table this library knows). *)
val length_for_country : string -> int option

(** Two check digits for a country and BBAN, used to build valid test IBANs. *)
val check_digits : country:string -> bban:string -> string
