(** Ledger postings for transfer transitions. Pure: every function takes the current
    {!Ledger.Balances.t} and returns the entries to book, already checked against it, so
    the caller appends them to its journal in order (or drops them).

    Outgoing transfer:
    - [Submitted]: a hold of [amount + fee] on the customer account, so the money is no
      longer available but still posted;
    - [Settled]: the hold is captured, [amount] to the rail's settlement account and
      [fee] to fee income (two entries, so the fee shows as its own line);
    - [Rejected], [Failed] or [Timed_out] after submission: the hold is released; a
      settlement that arrives after [Timed_out] is booked as a fresh debit (and fee);
    - [Returned] or [Recalled]: the amount comes back from the settlement account. The
      fee is kept, as the payment was still executed. A recall request or refusal
      moves no money.

    Incoming transfer, when it settles: the settlement account pays the customer
    account, then the fee (if any) moves from the customer to fee income. [Returned]
    and [Recalled] refund that fee and send the amount back; so does [Reorged] after
    the deposit was credited (it is credited again when it re-confirms).

    Every entry has the transfer id as [reference] and [<event id>/<leg>] as its
    idempotency key, where the event id is the provider event that caused the transition,
    so a webhook delivered twice is booked once. *)

open! Core

module Chart : sig
  (** Where the other side of each posting goes. *)
  type t =
    { settlement : Rail.t -> Money.Currency.t -> Ledger.Account_id.t
    (** the institution's position at the payment network: an asset that may go
        negative *)
    ; fee_income : Money.Currency.t -> Ledger.Account_id.t
    ; fx_position : Money.Currency.t -> Ledger.Account_id.t
    (** the institution's inventory in a currency, the other side of swaps *)
    }

  (** [settlement:<rail>/<currency>], [income:fees/<currency>] and
      [fx:position/<currency>]. *)
  val default : t

  (** The accounts a chart names for these rails and currencies, ready to open:
      settlement and FX position accounts are assets allowed to go negative, fee
      accounts are income. *)
  val accounts : t -> rails:Rail.t list -> currencies:Money.Currency.t list -> Ledger.Account.t list
end

(** The hold an outgoing transfer places: [transfer:<id>]. *)
val hold_id : Transfer.t -> Ledger.Hold_id.t

(** Entries for moving [transfer] from its current status to [to_]. Fails if the
    transition is not allowed or an entry breaks a ledger rule (an outgoing transfer
    over the available balance fails at [Submitted]). *)
val transition
  :  Chart.t
  -> Ledger.Balances.t
  -> Transfer.t
  -> to_:Transfer.Status.t
  -> event_id:string
  -> at:Time_ns.t
  -> Ledger.Entry.t list Or_error.t

(** Entries for a transfer as first seen: an incoming transfer that arrives [Settled]
    is booked at once; anything else books nothing yet. *)
val created
  :  Chart.t
  -> Ledger.Balances.t
  -> Transfer.t
  -> event_id:string
  -> at:Time_ns.t
  -> Ledger.Entry.t list Or_error.t

(** Applies entries to balances in order, as a journal would. *)
val apply : Ledger.Balances.t -> Ledger.Entry.t list -> Ledger.Balances.t Or_error.t

(** Appends entries to a journal in order. A replayed event returns [Duplicate]s. *)
val post
  :  Ledger.Journal.t
  -> Ledger.Entry.t list
  -> Ledger.Journal.Append_result.t list Or_error.t
