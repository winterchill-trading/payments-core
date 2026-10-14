(** US domestic scheme rules for ACH credits, the RTP network and Fedwire Funds: USD only,
    the amount limits, and the payment reference.

    Banks often set lower limits than the networks, so the RTP limit is an input
    ({!Rules}). Cut-off times, the Federal Reserve holiday calendar, ACH returns by R-code
    and NACHA file building are out of scope; R-codes travel as the four-character reason
    a provider reports. *)

open! Core

module Scheme : sig
  type t =
    | Ach
    | Rtp
    | Fedwire
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]

  val of_rail : Rail.t -> t option
end

(** The RTP network's per-payment limit: USD 10,000,000. *)
val rtp_network_max : Money.Amount.t

(** NACHA's per-payment limit for Same Day ACH: USD 1,000,000. *)
val same_day_ach_max : Money.Amount.t

module Rules : sig
  type t

  (** [rtp_max_amount] (default {!rtp_network_max}) must be a positive USD amount no
      larger than the network's. *)
  val create : ?rtp_max_amount:Money.Amount.t -> unit -> t Or_error.t

  val default : t
  val rtp_max_amount : t -> Money.Amount.t
end

module Details : sig
  module Stable : sig
    module V1 : sig
      type t =
        { scheme_ref : string option
        (** the network's id once submitted: ACH trace number, RTP message id, Fedwire
            IMAD *)
        ; effective_date : Date.Stable.V1.t option
        (** ACH: the settlement date asked for *)
        }
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  type t = Stable.V1.t =
    { scheme_ref : string option
    ; effective_date : Date.t option
    }
  [@@deriving bin_io, sexp, compare, equal, hash]

  val empty : t
end

(** Longest payment reference: 80 characters on ACH (one addenda record), 140 on RTP
    (unstructured remittance) and Fedwire (four lines of originator to beneficiary
    information). *)
val max_reference_length : Scheme.t -> int

(** Fails on a non-USD or non-positive amount, an RTP payment above the rules' limit, a
    Same Day ACH entry above {!same_day_ach_max}, or a reference that is too long or not
    printable ASCII. *)
val validate
  :  ?rules:Rules.t
  -> Rail.t
  -> amount:Money.Amount.t
  -> reference:string
  -> unit Or_error.t
