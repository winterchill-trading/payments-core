(** The network a transfer moves over. A rail decides which beneficiary formats are
    acceptable, whether a payment can come back after settling, and how long settlement
    normally takes. *)

open! Core

module Stable : sig
  module V2 : sig
    type t =
      | Sepa_credit (** SEPA Credit Transfer, settles next business day *)
      | Sepa_instant (** SEPA Instant Credit Transfer, seconds, around the clock *)
      | Onchain of Network.Stable.V1.t
      | Lightning
      | Intraledger (** between two accounts of the same institution *)
      | Card_scheme (** card authorisations and clearing, used by card-core *)
      | Faster_payments (** UK Faster Payments: seconds, around the clock, GBP *)
      | Chaps (** UK CHAPS: same-day gross settlement on business days, GBP *)
      | Bacs (** UK Bacs Direct Credit: three-day cycle, GBP *)
    [@@deriving bin_io, sexp, compare, equal, hash, quickcheck]
  end

  module V1 : sig
    (** The first six rails' wire format (digest [8dfb73bd…]), read into the current type.
        Writing [Faster_payments], [Chaps] or [Bacs] through it raises: write {!V2}
        instead. Its generator only produces the six rails it can hold. *)
    type t = V2.t [@@deriving bin_io, sexp, compare, equal, hash, quickcheck]
  end
end

type t = Stable.V2.t =
  | Sepa_credit
  | Sepa_instant
  | Onchain of Network.t
  | Lightning
  | Intraledger
  | Card_scheme
  | Faster_payments
  | Chaps
  | Bacs
[@@deriving sexp, compare, equal, hash, quickcheck]

(** [bin_io] on [t] is the {!Stable.V1} format, so that records which embed a rail keep
    their wire format and digests; it refuses the UK rails. Use {!Stable.V2} where a UK
    rail can occur. *)
include Binable.S with type t := t

val all : t list

include Stringable.S with type t := t

(** Settles within seconds rather than in a clearing cycle. *)
val is_instant : t -> bool

(** A settled payment can still come back (a SEPA return or recall, a card chargeback, a
    UK payment returned by the receiving bank). On-chain, Lightning and intraledger
    payments are final once settled. *)
val is_returnable : t -> bool

(** How long after submission a payment on this rail normally settles. A transfer still
    pending after this span is worth a look; it is not failed automatically. *)
val expected_settlement : t -> Time_ns.Span.t

(** The UK domestic rails: Faster Payments, CHAPS and Bacs. *)
val is_uk : t -> bool
