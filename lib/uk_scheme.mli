(** UK domestic scheme rules for Faster Payments, CHAPS and Bacs Direct Credit: the GBP
    amount limits, the payment reference, and when a CHAPS payment gets same-day value.

    Banks set their own limits under the schemes' (a Faster Payments limit far below the
    scheme's GBP 1 million is common) and their own CHAPS cut-off before the scheme's
    close, so both are inputs ({!Rules}). Bacs is modelled only as far as a draft needs:
    currency, reference and its three-day cycle in {!Rail.expected_settlement}; files,
    service user numbers and the cycle calendar are out of scope. *)

open! Core

module Scheme : sig
  type t =
    | Fps (** Faster Payments *)
    | Chaps
    | Bacs
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]

  val of_rail : Rail.t -> t option
  val to_rail : t -> Rail.t
end

(** The Faster Payments scheme's per-payment limit: GBP 1,000,000. *)
val fps_scheme_max : Money.Amount.t

(** Last instant to submit a CHAPS payment for value on a date: the scheme closes at 18:00
    UK time on weekdays. UK time is GMT, or BST (GMT+1) from the last Sunday of March to
    the last Sunday of October. Bank holidays are not known here: a bank's own hook should
    answer [None] for them. *)
val default_chaps_cutoff : Date.t -> Time_ns.t option

module Rules : sig
  type t

  (** [fps_max_amount] (default {!fps_scheme_max}) must be a positive GBP amount no larger
      than the scheme's. [chaps_cutoff date] is the last instant a CHAPS payment is
      accepted for value on [date], or [None] when [date] is not a CHAPS business day
      (default {!default_chaps_cutoff}). *)
  val create
    :  ?fps_max_amount:Money.Amount.t
    -> ?chaps_cutoff:(Date.t -> Time_ns.t option)
    -> unit
    -> t Or_error.t

  (** The scheme limits and {!default_chaps_cutoff}. *)
  val default : t

  val fps_max_amount : t -> Money.Amount.t
  val chaps_cutoff : t -> Date.t -> Time_ns.t option
end

module Details : sig
  module Stable : sig
    module V1 : sig
      type t =
        { scheme_ref : string option
        (** the scheme's id once submitted: the Faster Payments id, the CHAPS UETR *)
        ; value_date : Date.Stable.V1.t option (** CHAPS and Bacs: the settlement day *)
        }
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  type t = Stable.V1.t =
    { scheme_ref : string option
    ; value_date : Date.t option
    }
  [@@deriving bin_io, sexp, compare, equal, hash]

  (** Neither field known yet. *)
  val empty : t
end

(** Longest payment reference: 18 characters on Faster Payments and Bacs, 140 on CHAPS
    (the ISO 20022 unstructured remittance field). *)
val max_reference_length : Scheme.t -> int

(** Fails on a non-GBP or non-positive amount, a Faster Payment above the rules' limit, a
    reference that is too long, and reference characters the scheme does not carry: Bacs
    takes [A-Z 0-9 . & / -] and space (lower case is upper-cased by banks, so it is
    accepted), the others printable ASCII. *)
val validate
  :  ?rules:Rules.t
  -> Scheme.t
  -> amount:Money.Amount.t
  -> reference:string
  -> unit Or_error.t

(** The first day, from [now]'s UTC date on, whose CHAPS cut-off is not yet past: today
    before the cut-off, otherwise the next business day. Fails if no business day comes
    within two weeks (a hook that never answers). *)
val chaps_value_date : Rules.t -> now:Time_ns.t -> Date.t Or_error.t
