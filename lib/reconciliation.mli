(** Comparing what a provider reports with what the ledger says.

    Two levels: balances (per account and currency) and transfers (status per provider
    reference). Neither fixes anything; each difference is a {!Diff.t} for a person or a
    job to resolve, typically by booking a missed provider event.

    Balances are compared with the ledger state the caller passes. For a report "as of"
    an earlier time, fold the journal up to that time first. *)

open! Core

module Basis : sig
  (** Which ledger figure the provider's number corresponds to. *)
  type t =
    | Posted (** booked balance, holds included *)
    | Available (** spendable balance, holds deducted *)
  [@@deriving sexp, compare, equal]
end

module Reported_balance : sig
  type t =
    { account : Ledger.Account_id.t
    ; amount : Money.Amount.t
    ; basis : Basis.t
    }
  [@@deriving sexp, compare, equal]
end

module Reported_transfer : sig
  type t =
    { provider_ref : string
    ; status : Transfer.Status.t
    }
  [@@deriving sexp, compare, equal]
end

module Diff : sig
  type t =
    | Balance_mismatch of
        { account : Ledger.Account_id.t
        ; basis : Basis.t
        ; ledger : Money.Amount.t
        ; provider : Money.Amount.t
        ; delta : Money.Amount.t (** provider minus ledger *)
        }
    | Unknown_account of { account : Ledger.Account_id.t }
    (** reported, but not open in the ledger *)
    | Unreported_balance of
        { account : Ledger.Account_id.t
        ; ledger : Money.Amount.t
        }
    (** a non-zero ledger balance in scope the provider did not report *)
    | Status_mismatch of
        { transfer : Transfer.Id.t
        ; provider_ref : string
        ; ledger : Transfer.Status.t
        ; provider : Transfer.Status.t
        }
    | Unknown_transfer of { provider_ref : string; provider : Transfer.Status.t }
    (** the provider has it, we do not *)
    | Missing_at_provider of
        { transfer : Transfer.Id.t
        ; provider_ref : string
        }
  [@@deriving sexp, compare, equal]
end

(** One diff per reported balance that differs, then one per non-zero ledger balance in
    [scope] (accounts and currencies) the report left out. [scope] defaults to none. *)
val balances
  :  ?scope:(Ledger.Account_id.t * Money.Currency.t) list
  -> Ledger.Balances.t
  -> Reported_balance.t list
  -> Diff.t list

(** Matches by [provider_ref]. Transfers without one (never submitted) are skipped.
    [Missing_at_provider] is reported only for transfers in flight ([Submitted] or
    [Pending]), since providers usually list only a recent window. *)
val transfers : Transfer.t list -> Reported_transfer.t list -> Diff.t list
