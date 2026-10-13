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

(** Fails on a non-EUR amount, a charge bearer other than [SLEV], invalid parties or
    texts, debtor or creditor IBANs outside SEPA, or an SCT Inst amount over
    {!inst_max_amount}. *)
val validate : Scheme.t -> Details.t -> amount:Money.Amount.t -> unit Or_error.t

module Use : sig
  (** The R-transactions that carry a reason code. *)
  type t =
    | Reject (** before settlement (pacs.002 RJCT) *)
    | Return (** after settlement, by the beneficiary bank (pacs.004) *)
    | Recall (** request by the originator bank (camt.056) *)
    | Recall_refusal (** negative answer to a recall (camt.029) *)
  [@@deriving sexp, compare, equal, enumerate]
end

(** Reason codes the rulebook lists for a use. *)
val reasons : Scheme.t -> Use.t -> Iso_reason.t list

val check_reason : Scheme.t -> Use.t -> Iso_reason.t -> unit Or_error.t

module Inst : sig
  (** Target maximum execution time from the acceptance time stamp. *)
  val max_execution_time : Time_ns.Span.t

  (** After this, an originator bank without a confirmation treats the transfer as
      rejected (reason [AB05]). *)
  val timeout : Time_ns.Span.t

  val timed_out : acceptance_time:Time_ns.t -> now:Time_ns.t -> bool
end

module Deadline : sig
  (** Last business day for the beneficiary bank to return a settled SCT. *)
  val return : settled_on:Date.t -> Date.t

  (** Last day the originator bank may recall: 10 business days after settlement, or
      13 months for fraud ([FRAD]) and for customer-initiated recalls ([AC03], [AM09],
      [CUST]). *)
  val recall : settled_on:Date.t -> Iso_reason.t -> Date.t

  (** The beneficiary bank answers a recall within 15 business days. *)
  val recall_answer : received_on:Date.t -> Date.t
end
