(** A recurring outgoing payment: same beneficiary, amount and rail on a {!Schedule}.

    An active order is due once the current date (in the caller's zone) reaches
    [next_run]. Running it yields one {!Transfer} draft and moves [next_run] to the next
    schedule date after today. Runs missed while the process was down are not paid in a
    burst: one draft is produced, for the latest due date, and the schedule continues from
    today, the safer failure for the customer. Paused orders skip their dates; resuming
    picks up from the next date on or after the resume day. *)

open! Core
module Id : Opaque_id.S

module Status : sig
  type t =
    | Active
    | Paused
    | Cancelled
    | Finished (** the [until] date has passed *)
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Stable : sig
  module V2 : sig
    type t =
      { id : Id.Stable.V1.t
      ; account : Ledger.Account_id.Stable.V1.t
      ; beneficiary : Beneficiary.Stable.V2.t
      ; rail : Rail.Stable.V2.t
      ; amount : Money.Amount.Stable.V1.t
      ; reference : string
      ; details : Transfer.Details.Stable.V2.t option
      ; schedule : Schedule.t
      ; until : Date.Stable.V1.t option (** last date a run may fall on *)
      ; next_run : Date.Stable.V1.t
      ; status : Status.t
      ; runs : int
      ; last_run : Date.Stable.V1.t option
      }
    [@@deriving bin_io, sexp, compare, equal]
  end

  module V1 : sig
    (** The format before the UK rails: writing a UK rail, beneficiary or details
        raises. *)
    type t = V2.t =
      { id : Id.Stable.V1.t
      ; account : Ledger.Account_id.Stable.V1.t
      ; beneficiary : Beneficiary.Stable.V1.t
      ; rail : Rail.Stable.V1.t
      ; amount : Money.Amount.Stable.V1.t
      ; reference : string
      ; details : Transfer.Details.Stable.V1.t option
      ; schedule : Schedule.t
      ; until : Date.Stable.V1.t option
      ; next_run : Date.Stable.V1.t
      ; status : Status.t
      ; runs : int
      ; last_run : Date.Stable.V1.t option
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V2.t =
  { id : Id.t
  ; account : Ledger.Account_id.t
  ; beneficiary : Beneficiary.t
  ; rail : Rail.t
  ; amount : Money.Amount.t
  ; reference : string
  ; details : Transfer.Details.t option
  ; schedule : Schedule.t
  ; until : Date.t option
  ; next_run : Date.t
  ; status : Status.t
  ; runs : int
  ; last_run : Date.t option
  }
[@@deriving sexp, compare, equal]

(** The first run is the first schedule date on or after [start]. [details] is the
    template for each draft (required on SEPA rails); each run gets its own end-to-end id,
    [<order id>-<run date>]. Fails on an invalid schedule, a non-positive amount, a
    beneficiary the rail cannot reach, details the rail does not accept, or an [until]
    before the first run. *)
val create
  :  id:Id.t
  -> account:Ledger.Account_id.t
  -> beneficiary:Beneficiary.t
  -> rail:Rail.t
  -> amount:Money.Amount.t
  -> ?reference:string
  -> ?details:Transfer.Details.t
  -> schedule:Schedule.t
  -> start:Date.t
  -> ?until:Date.t
  -> unit
  -> t Or_error.t

val pause : t -> t Or_error.t
val resume : t -> today:Date.t -> t Or_error.t
val cancel : t -> t
val is_due : t -> now:Time_ns.t -> zone:Time_float.Zone.t -> bool

(** [None] when not due. The first run's draft needs SCA; later drafts carry the
    recurring-payment exemption (RTS art. 14). The draft's id is [<order id>:<run date>],
    so running the same date twice (a retry) yields the same transfer id for the caller to
    de-duplicate. *)
val run
  :  t
  -> now:Time_ns.t
  -> zone:Time_float.Zone.t
  -> (t * Transfer.t) option Or_error.t

(** [run] at the time source's current time. *)
val poll
  :  t
  -> time_source:[> read ] Async_kernel.Time_source.T1.t
  -> zone:Time_float.Zone.t
  -> (t * Transfer.t) option Or_error.t
