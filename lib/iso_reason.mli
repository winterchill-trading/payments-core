(** ISO 20022 external reason codes used in SEPA rejects, returns, recalls and answers to
    recalls (ExternalStatusReason1Code, ExternalReturnReason1Code,
    ExternalCancellationReason1Code). Codes the rulebooks list are constructors;
    anything else a counterparty sends is kept as [Other] with its four-character code
    so nothing is lost, and {!Sepa.check_reason} flags it. *)

open! Core

type t =
  | AB05 (** timeout at the creditor agent (SCT Inst) *)
  | AB06 (** timeout at an instructed agent (SCT Inst) *)
  | AB07 (** offline agent (SCT Inst) *)
  | AB08 (** offline creditor agent (SCT Inst) *)
  | AB09 (** error at the creditor agent (SCT Inst) *)
  | AB10 (** error at an instructed agent (SCT Inst) *)
  | AC01 (** incorrect account number *)
  | AC03 (** wrong creditor account (recall: wrong IBAN entered by originator) *)
  | AC04 (** closed account number *)
  | AC06 (** blocked account *)
  | AG01 (** transaction forbidden on this account *)
  | AG02 (** invalid bank operation code *)
  | AM02 (** amount above the scheme maximum; retired for SCT Inst in 2025 *)
  | AM04 (** insufficient funds (also a negative answer to a recall) *)
  | AM05 (** duplication *)
  | AM09 (** wrong amount (recall) *)
  | AM23 (** amount exceeds the settlement limit *)
  | ARDT (** already returned (negative answer to a recall) *)
  | BE04 (** missing creditor address *)
  | CNOR (** creditor bank not registered *)
  | CUST (** requested by the customer (recall, or refusal by the beneficiary) *)
  | DNOR (** debtor bank not registered *)
  | DUPL (** duplicate payment (recall) *)
  | ERIN (** ERI option not supported *)
  | FF01 (** invalid file format *)
  | FOCR (** following a cancellation request: a return answering a recall *)
  | FRAD (** fraudulent origin (recall) *)
  | LEGL (** legal decision (negative answer to a recall) *)
  | MD07 (** end customer deceased *)
  | MS02 (** refused by the beneficiary (not specified, customer generated) *)
  | MS03 (** not specified, agent generated *)
  | NOAS (** no answer from the customer (negative answer to a recall) *)
  | NOOR (** original transaction never received (negative answer to a recall) *)
  | RC01 (** bank identifier incorrect *)
  | RR01 (** missing debtor account or identification *)
  | RR02 (** missing debtor name or address *)
  | RR03 (** missing creditor name or address *)
  | RR04 (** regulatory reason *)
  | TECH (** technical problem producing erroneous transfers (recall) *)
  | TM01 (** received after the cut-off time *)
  | Other of string (** any other four-character code, upper case *)
[@@deriving bin_io, sexp, compare, equal, hash]

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

(** The rulebook codes, without [Other]. *)
val known : t list

val to_code : t -> string

(** Upper-cases; a code not in {!known} becomes [Other]. Fails unless it is four
    alphanumeric characters. *)
val of_code : string -> t Or_error.t

val description : t -> string

module Kind : sig
  (** Where a code may appear (EPC135-18 / EPC059-18 v6.0). *)
  type t =
    | Reject
    | Return
    | Recall
    | Rfro (** request for recall by the originator *)
    | Answer (** answer to a recall or RFRO *)
  [@@deriving sexp, compare, equal, enumerate]
end

(** Uses the rulebooks allow for this code; empty for [Other] and retired codes. *)
val kinds : t -> Kind.t list

(** The customer can fix it (wrong, closed or blocked account, missing address,
    deceased or refusing beneficiary), as opposed to a bank or scheme problem. *)
val is_customer_error : t -> bool
