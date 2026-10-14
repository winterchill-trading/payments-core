(** UK account number validation by modulus checking, as published by the UK payment
    scheme operator in "Validating account numbers" (version 8 of the specification).

    The check needs two data files that the operator publishes and revises several times a
    year: the weight table ([valacdos.txt]: sort code ranges, a method, fourteen weights
    and an exception number) and the sort code substitution table ([scsubtab.txt], used by
    exception 5). Neither is bundled here, since a stale copy silently rejects new sort
    codes: load the current files with {!Table.of_string}.

    A pass only means the account number could exist at that sort code; a sort code the
    table does not list cannot be checked, which the specification says to treat as valid
    ({!Result.Not_checked}). Account numbers are the standard eight digits; six and seven
    digits are zero padded. Nine and ten digit numbers have bank-specific conversions and
    are refused. *)

open! Core

module Method : sig
  type t =
    | Mod10
    | Mod11
    | Dblal (** double alternate: sum of the digits of each product, modulus 10 *)
  [@@deriving sexp, compare, equal, enumerate]
end

module Rule : sig
  (** One line of the weight table. *)
  type t =
    { first : Sort_code.t
    ; last : Sort_code.t
    ; method_ : Method.t
    ; weights : int list (** fourteen, for [u v w x y z a b c d e f g h] *)
    ; exception_ : int option (** 1 to 14 *)
    }
  [@@deriving sexp, compare, equal]

  (** A line in the published format: [089000 089999 MOD11 0 0 0 0 0 0 8 7 6 5 4 3 2 1],
      optionally followed by the exception number. *)
  val of_string : string -> t Or_error.t
end

module Table : sig
  type t

  (** No rules: every sort code is [Not_checked]. *)
  val empty : t

  val create : ?substitutions:(Sort_code.t * Sort_code.t) list -> Rule.t list -> t

  (** The weight table's text, and the substitution table's text (lines of two sort
      codes). Blank lines are skipped. *)
  val of_string : ?substitutions:string -> string -> t Or_error.t

  (** The rules for a sort code, in table order (none, one or two). *)
  val rules : t -> Sort_code.t -> Rule.t list
end

module Result : sig
  type t =
    | Valid
    | Invalid
    | Not_checked (** the table has no rule for the sort code *)
  [@@deriving sexp, compare, equal, enumerate]
end

(** Pads a six or seven digit account number to eight digits; refuses anything else that
    is not eight digits. *)
val normalize_account_number : string -> string Or_error.t

val check
  :  Table.t
  -> sort_code:Sort_code.t
  -> account_number:string
  -> Result.t Or_error.t
