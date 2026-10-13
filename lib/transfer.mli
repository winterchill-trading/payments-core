(** A payment between a ledger account and the outside world (or another ledger
    account), with its lifecycle.

    {v
    Draft ─> Awaiting_sca ─> Submitted ─> Pending ─> Settled ─> Returned
      │           │              │           │          │
      │           │              │           │          └─> Recall_requested ─> Recalled
      │           │              │           │                      │      └──> Returned
      │           │              │           │                      └─> Recall_refused
      │           │              ├───────────┴─> Rejected (scheme reason code)
      │           │              └─> Timed_out ─> Settled (late) | Rejected   (SCT Inst)
      └───────────┴─> Cancelled                  Failed (our side: funds, SCA, expiry)
    v}

    [Rejected], [Returned], [Recall_requested] and [Recall_refused] carry ISO 20022
    reason codes; on SEPA rails the code must be one the rulebook allows for that use
    ({!Sepa.check_reason}). Returns and recalls exist only on returnable rails, and SCT
    Inst has no returns except the [FOCR] answer to a recall. [Timed_out] exists only on
    SCT Inst; [Reorged] only on-chain (Pending or Settled -> Reorged -> Pending, when a
    block is orphaned and the transaction must confirm again; outgoing transfers only
    from Pending, while the hold is still open). Incoming transfers start
    at [Pending] or [Settled]. Every transition is timestamped.

    Rail-specific data lives in {!Details}: the pacs.008 fields for SEPA, the travel-rule
    record for on-chain transfers, the payment hash for Lightning. *)

open! Core

module Id : Opaque_id.S

module Direction : sig
  type t =
    | In
    | Out
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
end

module Failure_reason : sig
  (** Failures on our side, before or outside the scheme. *)
  type t =
    | Insufficient_funds
    | Sca_failed
    | Expired (** e.g. a Lightning invoice ran out before payment *)
    | Provider_error of string (** the provider refused the request itself *)
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Status : sig
  type t =
    | Draft
    | Awaiting_sca
    | Submitted
    | Pending
    | Settled
    | Rejected of Iso_reason.Stable.V1.t
    | Failed of Failure_reason.t
    | Returned of Iso_reason.Stable.V1.t
    | Recall_requested of Iso_reason.Stable.V1.t
    | Recall_refused of Iso_reason.Stable.V1.t
    | Recalled (** the recall was accepted and the funds came back ([FOCR]) *)
    | Cancelled
    | Timed_out
    (** SCT Inst: no confirmation within {!Sepa.Inst.timeout}; the payer's account is
        restored, but a late settlement can still arrive *)
    | Reorged (** on-chain: the block holding the transaction was orphaned *)
  [@@deriving bin_io, sexp, compare, equal, hash]

  (** No transition leaves this status, whatever the rail. *)
  val is_final : t -> bool

  (** The constructor name, for tables and logs. *)
  val name : t -> string
end

(** Every allowed [(from, to)] pair by constructor name, before rail restrictions. *)
val allowed_transitions : (string * string list) list

module Counterparty : sig
  type t =
    | Beneficiary of Beneficiary.Stable.V1.t (** outgoing: who we pay *)
    | External of
        { name : string
        ; detail : string (** IBAN, address or invoice the funds came from *)
        }
    (** incoming: who paid us, as reported *)
  [@@deriving bin_io, sexp, compare, equal]
end

module Details : sig
  type t =
    | Sepa of Sepa.Details.Stable.V1.t
    | Onchain of
        { travel_rule : Travel_rule.Stable.V1.t option
        ; tx_hash : string option
        ; block_hash : string option
        ; confirmations : int
        ; finalized : bool
        }
    | Lightning of
        { payment_hash : string option
        ; preimage : string option (** proof of payment, once settled *)
        }
    | Intraledger
    | Card
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
      ; reference : string (** free text shown to the customer *)
      ; details : Details.t
      ; status : Status.t
      ; history : (Status.t * Utc_time.Stable.V1.t) list (** newest first *)
      ; authorization : Sca.Authorization.t option
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
  ; details : Details.t
  ; status : Status.t
  ; history : (Status.t * Time_ns.t) list
  ; authorization : Sca.Authorization.t option
  ; provider_ref : string option
  }
[@@deriving sexp, compare, equal]

(** An outgoing draft. Fails on a non-positive amount, a negative fee or one in another
    currency, a beneficiary the rail cannot reach, or details that do not fit the rail:
    SEPA rails need valid {!Sepa.Details} whose creditor IBAN is the beneficiary's;
    on-chain transfers need a travel-rule record that passes {!Travel_rule.check}
    ([eur_value] is the amount's EUR value for the self-hosted wallet threshold). Other
    rails get their details filled in when omitted. *)
val draft
  :  id:Id.t
  -> rail:Rail.t
  -> account:Ledger.Account_id.t
  -> beneficiary:Beneficiary.t
  -> amount:Money.Amount.t
  -> ?fee:Money.Amount.t
  -> ?reference:string
  -> ?details:Details.t
  -> ?eur_value:Money.Amount.t
  -> now:Time_ns.t
  -> unit
  -> t Or_error.t

(** An incoming payment as first reported: [Pending] or [Settled]. Fails like {!draft}
    on amounts and details, and when the fee exceeds the amount. *)
val incoming
  :  id:Id.t
  -> rail:Rail.t
  -> account:Ledger.Account_id.t
  -> from:Counterparty.t
  -> amount:Money.Amount.t
  -> ?fee:Money.Amount.t
  -> ?reference:string
  -> ?details:Details.t
  -> ?provider_ref:string
  -> status:Status.t
  -> now:Time_ns.t
  -> unit
  -> t Or_error.t

val can_transition : t -> Status.t -> bool

(** Moves to [to_] if the table, the rail and (on SEPA rails) the rulebook's reason
    codes allow it. Repeating the current status is an error, so callers notice a
    duplicate event instead of stamping it twice. *)
val transition : t -> to_:Status.t -> at:Time_ns.t -> t Or_error.t

(** Moves to [Awaiting_sca] and records the challenge as the authorization. *)
val require_sca : t -> challenge:Sca.Id.t -> at:Time_ns.t -> t Or_error.t

(** Records that SCA was skipped under [exemption] (its RTS article goes with it). *)
val exempt : t -> exemption:Sca.Exemption.t -> t Or_error.t

(** Submits; for SEPA the acceptance time stamp is set to [at]. An outgoing transfer
    needs an authorization first ({!require_sca} with a verified challenge, or
    {!exempt}). *)
val submit : t -> ?provider_ref:string -> at:Time_ns.t -> unit -> t Or_error.t

(** Applies a pacs.002 status report: [RCVD]/[ACTC]/[ACCP] keep [Submitted],
    [ACSP]/[ACWC]/[PDNG] move to [Pending], [ACSC]/[ACCC] to [Settled], [RJCT] to
    [Rejected reason] ([reason] is then required). A report that does not change the
    status only records [last_status]. *)
val apply_status_report
  :  t
  -> Iso20022.Transaction_status.t
  -> ?reason:Iso_reason.t
  -> ?value_date:Date.t
  -> at:Time_ns.t
  -> unit
  -> t Or_error.t

(** SCT Inst: a submitted transfer without a confirmation after {!Sepa.Inst.timeout}
    becomes [Timed_out]. [None] when nothing changes. *)
val check_inst_timeout : t -> now:Time_ns.t -> t option

(** [Recall_requested reason], checked against the rulebook deadline for the reason
    (counted from the settlement date in UTC). *)
val request_recall : t -> reason:Iso_reason.t -> at:Time_ns.t -> t Or_error.t

(** Lightning: settles an outgoing payment on the preimage whose SHA-256 is the
    invoice's payment hash, and keeps it as proof of payment. *)
val settle_lightning : t -> preimage:string -> at:Time_ns.t -> t Or_error.t

(** On-chain: records the confirmation count (and whether the block is finalized) for
    the transaction [tx_hash] in [block_hash]. An incoming [Pending] transfer settles once
    {!Confirmation_policy.required} is met; [eur_value] is its EUR value for the policy. *)
val record_confirmations
  :  t
  -> tx_hash:string
  -> block_hash:string
  -> confirmations:int
  -> finalized:bool
  -> ?eur_value:Money.Amount.t
  -> at:Time_ns.t
  -> unit
  -> t Or_error.t

(** No further transition is possible on this transfer's rail. *)
val is_terminal : t -> bool

(** [amount + fee]: what leaves the customer account on an outgoing transfer. *)
val total_debit : t -> Money.Amount.t

(** When the transfer first entered a status with this constructor name. *)
val entered_at : t -> string -> Time_ns.t option
