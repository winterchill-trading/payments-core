(** A payment between a ledger account and the outside world (or another ledger
    account), with its lifecycle.

    {v
    Draft ──> Awaiting_sca ──> Submitted ──> Pending ──> Settled ──> Returned
      │            │               │            │
      └────────────┴──> Cancelled  └────────────┴──> Failed
    v}

    Draft and Awaiting_sca can also fail (an SCA lockout, a rejected beneficiary).
    Incoming transfers start at [Pending] or [Settled]. Only rails where payments can
    come back ({!Rail.is_returnable}) allow [Settled -> Returned]. Every transition is
    timestamped in [history]. *)

open! Core

module Id : Opaque_id.S

module Direction : sig
  type t =
    | In
    | Out
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
end

module Failure_reason : sig
  type t =
    | Insufficient_funds
    | Sca_failed
    | Beneficiary_rejected (** the receiving side refused or does not exist *)
    | Expired (** e.g. a Lightning invoice ran out before payment *)
    | Provider_rejected of string
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Status : sig
  type t =
    | Draft
    | Awaiting_sca
    | Submitted
    | Pending
    | Settled
    | Failed of Failure_reason.t
    | Returned
    | Cancelled
  [@@deriving bin_io, sexp, compare, equal, hash]

  (** Settled is not terminal on returnable rails; see {!Transfer.is_terminal}. *)
  val is_final : t -> bool

  (** The constructor name, for tables and logs. *)
  val name : t -> string
end

(** Every allowed [(from, to)] pair by constructor name, ignoring the rail. *)
val allowed_transitions : (string * string list) list

module Counterparty : sig
  type t =
    | Beneficiary of Beneficiary.t (** outgoing: who we pay *)
    | External of
        { name : string
        ; detail : string (** IBAN, address or invoice the funds came from *)
        }
    (** incoming: who paid us, as reported *)
  [@@deriving bin_io, sexp, compare, equal]
end

module Stable : sig
  module V1 : sig
    type t =
      { id : Id.Stable.V1.t
      ; direction : Direction.t
      ; rail : Rail.Stable.V1.t
      ; account : Ledger.Account_id.Stable.V1.t
      (** the customer account debited (Out) or credited (In) *)
      ; counterparty : Counterparty.t
      ; amount : Money.Amount.Stable.V1.t (** positive, what the counterparty receives *)
      ; fee : Money.Amount.Stable.V1.t (** same currency, zero when free *)
      ; reference : string (** remittance information shown to both sides *)
      ; status : Status.t
      ; history : (Status.t * Utc_time.Stable.V1.t) list (** newest first *)
      ; sca : Sca.Id.Stable.V1.t option
      ; provider_ref : string option (** the provider's id, once submitted *)
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { id : Id.t
  ; direction : Direction.t
  ; rail : Rail.t
  ; account : Ledger.Account_id.t
  ; counterparty : Counterparty.t
  ; amount : Money.Amount.t
  ; fee : Money.Amount.t
  ; reference : string
  ; status : Status.t
  ; history : (Status.t * Time_ns.t) list
  ; sca : Sca.Id.t option
  ; provider_ref : string option
  }
[@@deriving sexp, compare, equal]

(** An outgoing draft. Fails on a non-positive amount, a negative fee or one in another
    currency, or a beneficiary the rail cannot reach. *)
val draft
  :  id:Id.t
  -> rail:Rail.t
  -> account:Ledger.Account_id.t
  -> beneficiary:Beneficiary.t
  -> amount:Money.Amount.t
  -> ?fee:Money.Amount.t
  -> ?reference:string
  -> now:Time_ns.t
  -> unit
  -> t Or_error.t

(** An incoming payment as first reported: [Pending] or [Settled]. *)
val incoming
  :  id:Id.t
  -> rail:Rail.t
  -> account:Ledger.Account_id.t
  -> from:Counterparty.t
  -> amount:Money.Amount.t
  -> ?fee:Money.Amount.t
  -> ?reference:string
  -> ?provider_ref:string
  -> status:Status.t
  -> now:Time_ns.t
  -> unit
  -> t Or_error.t

val can_transition : t -> Status.t -> bool

(** Moves to [to_] if the table and the rail allow it. Repeating the current status is an
    error, so callers notice a duplicate event instead of stamping it twice. *)
val transition : t -> to_:Status.t -> at:Time_ns.t -> t Or_error.t

val require_sca : t -> challenge:Sca.Id.t -> at:Time_ns.t -> t Or_error.t
val submit : t -> ?provider_ref:string -> at:Time_ns.t -> unit -> t Or_error.t

(** No further transition is possible on this transfer's rail. *)
val is_terminal : t -> bool

(** [amount + fee]: what leaves the customer account on an outgoing transfer. *)
val total_debit : t -> Money.Amount.t

(** When the transfer entered [status], if it did. *)
val entered_at : t -> Status.t -> Time_ns.t option
