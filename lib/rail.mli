(** The network a transfer moves over. A rail decides which beneficiary formats are
    acceptable, whether a payment can come back after settling, and how long settlement
    normally takes. *)

open! Core

module Stable : sig
  module V1 : sig
    type t =
      | Sepa_credit (** SEPA Credit Transfer, settles next business day *)
      | Sepa_instant (** SEPA Instant Credit Transfer, seconds, around the clock *)
      | Onchain of Network.Stable.V1.t
      | Lightning
      | Intraledger (** between two accounts of the same institution *)
      | Card_scheme (** card authorisations and clearing, used by card-core *)
    [@@deriving bin_io, sexp, compare, equal, hash, quickcheck]
  end
end

type t = Stable.V1.t =
  | Sepa_credit
  | Sepa_instant
  | Onchain of Network.t
  | Lightning
  | Intraledger
  | Card_scheme
[@@deriving bin_io, sexp, compare, equal, hash, quickcheck]

val all : t list

include Stringable.S with type t := t

(** Settles within seconds rather than in a clearing cycle. *)
val is_instant : t -> bool

(** A settled payment can still come back (a SEPA return or recall, a card chargeback).
    On-chain, Lightning and intraledger payments are final once settled. *)
val is_returnable : t -> bool

(** How long after submission a payment on this rail normally settles. A transfer
    still pending after this span is worth a look; it is not failed automatically. *)
val expected_settlement : t -> Time_ns.Span.t
