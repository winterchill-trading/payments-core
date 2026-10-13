(** Comparing what a provider reports with what the ledger says.

    Two levels: balances (per account and currency) and transfers (status per provider
    reference). Neither fixes anything; each difference is a {!Diff.t} for a person or a
    job to resolve, typically by booking a missed provider event.

    Balances are compared with the ledger state the caller passes. For a report "as of" an
    earlier time, fold the journal up to that time first. *)

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
        } (** a non-zero ledger balance in scope the provider did not report *)
    | Status_mismatch of
        { transfer : Transfer.Id.t
        ; provider_ref : string
        ; ledger : Transfer.Status.t
        ; provider : Transfer.Status.t
        }
    | Unknown_transfer of
        { provider_ref : string
        ; provider : Transfer.Status.t
        } (** the provider has it, we do not *)
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

(** {2 Transaction matching and breaks}

    The daily job: match our booked transactions with the provider's on the provider id,
    record every difference as a {!Break}, carry breaks across runs keeping the day they
    were first seen, and age them in business days. A break is resolved only by an
    adjustment entry in the ledger, made by one operator and approved by another. *)

module Internal_tx : sig
  type t =
    { provider_ref : string
    ; amount : Money.Amount.t
    ; booked_on : Date.t
    }
  [@@deriving sexp, compare, equal]
end

module External_tx : sig
  type t =
    { provider_ref : string
    ; amount : Money.Amount.t
    ; value_date : Date.t
    }
  [@@deriving sexp, compare, equal]
end

module Break : sig
  module Kind : sig
    type t =
      | Timing (** one side is still expected to catch up *)
      | Missing_internal (** the provider has it, we never booked it *)
      | Missing_external (** we booked it, the provider has nothing *)
      | Amount_mismatch
      | Duplicate (** the same provider id booked twice *)
    [@@deriving sexp, compare, equal, enumerate]
  end

  module Status : sig
    type t =
      | Open
      | Explained of string
      | Resolved of
          { adjustment : Ledger.Idempotency_key.t
          ; maker : string
          ; checker : string
          }
    [@@deriving sexp, compare, equal]
  end

  type t =
    { id : string (** [<kind>:<provider ref>], stable across runs *)
    ; kind : Kind.t
    ; provider_ref : string
    ; internal : Money.Amount.t option
    ; external_ : Money.Amount.t option
    ; first_seen : Date.t
    ; status : Status.t
    }
  [@@deriving sexp, compare, equal]

  module Age : sig
    type t =
      | Days_0_1
      | Days_2_5
      | Over_5
    [@@deriving sexp, compare, equal, enumerate]
  end

  (** Age in TARGET business days since [first_seen]. *)
  val age : t -> today:Date.t -> Age.t

  val explain : t -> string -> t

  (** Resolves with a booked adjustment entry whose reference names the break id. Fails
      when maker and checker are the same person or the entry does not reference the
      break. *)
  val resolve
    :  t
    -> adjustment:Ledger.Entry.t
    -> maker:string
    -> checker:string
    -> t Or_error.t
end

(** Breaks for one business day. A side missing for at most [timing_days] business days
    (default 1) is a [Timing] break rather than a missing one. *)
val match_transactions
  :  ?timing_days:int
  -> business_date:Date.t
  -> Internal_tx.t list
  -> External_tx.t list
  -> Break.t list

(** Merges a new run into the open breaks: a break seen before keeps its [first_seen] and
    status (a [Timing] break that became a missing one keeps its date); a [Timing] break
    absent from the new run has cleared by itself and is dropped; other open breaks stay
    until resolved, even when the run no longer reports them. *)
val carry_forward : previous:Break.t list -> current:Break.t list -> Break.t list

(** Open breaks per age bucket. *)
val ageing : Break.t list -> today:Date.t -> (Break.Age.t * int) list
