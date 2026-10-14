(** A UK domestic account: sort code and eight-digit account number, plus the roll number
    some building societies still use to tell their customers apart inside one collection
    account. This is what Faster Payments, CHAPS, Bacs and Confirmation of Payee address.

    A GB IBAN carries the sort code and account number after the four-letter bank code
    ([GB29 NWBK 601613 31926819]), so {!of_iban} needs no lookup. The other direction
    needs the bank code, which a sort code alone does not give, so no IBAN is derived
    here. *)

open! Core

module Stable : sig
  module V1 : sig
    type t = private
      { sort_code : Sort_code.Stable.V1.t
      ; account_number : string (** eight digits *)
      ; roll_number : string option
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

type t = Stable.V1.t = private
  { sort_code : Sort_code.t
  ; account_number : string
  ; roll_number : string option
  }
[@@deriving sexp, compare, equal, hash, quickcheck]

include Binable.S with type t := t

(** Six and seven digit account numbers are zero padded. With [modulus], an account number
    the table rejects is an error; a sort code the table does not list passes (the
    published rules say it cannot be checked). A roll number has 1 to 18 letters, digits,
    spaces, [/], [-] or [.]. *)
val create
  :  ?modulus:Uk_modulus.Table.t
  -> ?roll_number:string
  -> sort_code:Sort_code.t
  -> account_number:string
  -> unit
  -> t Or_error.t

(** GB IBANs only. *)
val of_iban : Iban.t -> t Or_error.t

(** A GB IBAN, or a sort code and account number separated by spaces or a slash
    ([04-00-04 12345678], [040004/12345678]). *)
val of_string : ?modulus:Uk_modulus.Table.t -> string -> t Or_error.t

(** [04-00-04 12345678], with [roll <roll number>] appended when there is one. *)
val to_string : t -> string
