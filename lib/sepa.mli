(** SEPA Credit Transfer (SCT) and SEPA Instant Credit Transfer (SCT Inst) scheme rules
    from the EPC rulebooks: the pacs.008 fields a transfer carries, which reason codes
    each R-transaction may use, the SCT Inst timing, and the deadlines for returns,
    recalls and answers to recalls (in TARGET business days). *)

open! Core

module Scheme : sig
  type t =
    | Sct
    | Sct_inst
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]

  val of_rail : Rail.t -> t option
end

module Details : sig
  module Stable : sig
    module V1 : sig
      type t =
        { payment_id : Iso20022.Payment_id.t
        ; debtor : Iso20022.Party.t
        ; creditor : Iso20022.Party.t
        ; remittance : Iso20022.Remittance.t
        ; charge_bearer : Iso20022.Charge_bearer.t
        ; purpose : string option (** ISO ExternalPurpose1Code, e.g. [SALA] *)
        ; requested_execution_date : Date.Stable.V1.t option
        ; acceptance_time : Utc_time.Stable.V1.t option
        (** SCT Inst time stamp from which the execution clock runs *)
        ; value_date : Date.Stable.V1.t option (** interbank settlement date *)
        ; booking_date : Date.Stable.V1.t option (** booked on the customer account *)
        ; last_status : Iso20022.Transaction_status.t option (** latest pacs.002 *)
        }
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  type t = Stable.V1.t =
    { payment_id : Iso20022.Payment_id.t
    ; debtor : Iso20022.Party.t
    ; creditor : Iso20022.Party.t
    ; remittance : Iso20022.Remittance.t
    ; charge_bearer : Iso20022.Charge_bearer.t
    ; purpose : string option
    ; requested_execution_date : Date.t option
    ; acceptance_time : Time_ns.t option
    ; value_date : Date.t option
    ; booking_date : Date.t option
    ; last_status : Iso20022.Transaction_status.t option
    }
  [@@deriving bin_io, sexp, compare, equal, hash]

  (** Charge bearer [SLEV], as the scheme requires. *)
  val create
    :  payment_id:Iso20022.Payment_id.t
    -> debtor:Iso20022.Party.t
    -> creditor:Iso20022.Party.t
    -> ?remittance:Iso20022.Remittance.t
    -> ?purpose:string
    -> ?requested_execution_date:Date.t
    -> unit
    -> t
end

(** Highest SCT Inst amount the scheme allows, if any. *)
val inst_max_amount : Money.Amount.t option

(** The scheme's amount field range (AT-T002): 0.01 to 999 999 999.99 EUR. *)
val max_amount : Money.Amount.t

(** Fails on a non-EUR amount, one outside 0.01 .. {!max_amount}, a charge bearer other
    than [SLEV], invalid parties or texts, debtor or creditor IBANs outside SEPA, or an
    SCT Inst amount over {!inst_max_amount}. *)
val validate : Scheme.t -> Details.t -> amount:Money.Amount.t -> unit Or_error.t

module Use : sig
  (** The R-transactions that carry a reason code. *)
  type t =
    | Reject (** before settlement (pacs.002 RJCT) *)
    | Return (** after settlement, by the beneficiary bank (pacs.004) *)
    | Recall (** recall by the originator bank (camt.056: DUPL, TECH, FRAD) *)
    | Rfro
    (** request for recall by the originator, i.e. the customer's own mistake (camt.056:
        AC03, AM09, CUST); the beneficiary must consent *)
    | Recall_refusal (** negative answer to a recall or RFRO (camt.029) *)
  [@@deriving sexp, compare, equal, enumerate]
end

(** Reason codes the rulebook lists for a use. SCT Inst has no returns after acceptance:
    its only pacs.004 is the positive answer to a recall ([FOCR]). *)
val reasons : Scheme.t -> Use.t -> Iso_reason.t list

val check_reason : Scheme.t -> Use.t -> Iso_reason.t -> unit Or_error.t

module Inst : sig
  (** Rulebook target for the whole round trip (2025 rulebook, 5-7-9 s sub-timeline). *)
  val target : Time_ns.Span.t

  (** Hard limit from the acceptance time stamp (AT-T056): without a confirmation by then
      the payer's account is restored (Instant Payments Regulation art. 5a). *)
  val timeout : Time_ns.Span.t

  (** When to ask the scheme what happened (pacs.028): two seconds after the timeout. *)
  val investigate_after : Time_ns.Span.t

  val timed_out : acceptance_time:Time_ns.t -> now:Time_ns.t -> bool
end

module Deadline : sig
  (** Last business day for the beneficiary bank to return a settled SCT. *)
  val return : settled_on:Date.t -> Date.t

  (** Last day the originator bank may send a recall: 10 business days after settlement
      for [DUPL] and [TECH], 13 months for [FRAD]. The rulebook window for a request for
      recall by the originator ([AC03], [AM09], [CUST]) was not confirmed; this uses 13
      months as well. *)
  val recall : settled_on:Date.t -> Iso_reason.t -> Date.t

  (** The beneficiary bank answers a recall within 15 business days. *)
  val recall_answer : received_on:Date.t -> Date.t
end
