(** Currency swaps (EUR to BTC, USDC to EUR, ...) against the institution's own inventory.

    A {!Quote} fixes what the customer gives and gets for a short time. Executing it books
    three entries, one currency each: the sell leg (customer to the FX position in the
    sold currency), the fee (customer to fee income, in the sold currency) and the buy leg
    (FX position to customer in the bought currency). Every amount is exact; the bought
    amount is rounded down to a minor unit, so rounding never favours the customer at the
    institution's expense or the reverse by more than one minor unit. *)

open! Core
module Id : Opaque_id.S

module Quote : sig
  type t =
    { id : Id.t
    ; sell : Money.Amount.t (** debited from the customer in total, fee included *)
    ; fee : Money.Amount.t (** part of [sell], same currency *)
    ; buy : Money.Amount.t (** credited to the customer *)
    ; market : Money.Rate.t (** the reference rate, sell currency as base *)
    ; spread_bps : int
    ; created_at : Time_ns.t
    ; expires_at : Time_ns.t
    }
  [@@deriving sexp_of, compare, equal]

  (** [market] may be quoted either way round; it is inverted when its base is the bought
      currency. The customer price is the market price less [spread_bps], and [fee]
      (default none) is charged on [sell]. Defaults: 30 second validity, no spread. Fails
      when the rate does not connect the two currencies, the amount is not positive, or
      the fee eats the whole amount. *)
  val create
    :  id:Id.t
    -> market:Money.Rate.t
    -> sell:Money.Amount.t
    -> buy:Money.Currency.t
    -> ?spread_bps:int
    -> ?fee:Money.Fee.t
    -> now:Time_ns.t
    -> ?ttl:Time_ns.Span.t
    -> unit
    -> t Or_error.t

  (** The effective price the customer gets: [buy / sell] in major units. *)
  val effective_price : t -> Q.t

  val is_expired : t -> now:Time_ns.t -> bool
end

(** Entries for executing [quote] at [now]: the sold currency leaves [from_], the bought
    currency arrives in [to_] (they may be the same multi-currency account). Fails on an
    expired quote or when the customer lacks the funds. *)
val execute
  :  Booking.Chart.t
  -> Ledger.Balances.t
  -> Quote.t
  -> from_:Ledger.Account_id.t
  -> to_:Ledger.Account_id.t
  -> event_id:string
  -> now:Time_ns.t
  -> Ledger.Entry.t list Or_error.t
