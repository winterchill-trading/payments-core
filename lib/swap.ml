open! Core
module Amount = Money.Amount
module Currency = Money.Currency

module Id =
  Opaque_id.Make
    (struct
      let module_name = "Payments_core.Swap.Id"
    end)
    ()

module Quote = struct
  type t =
    { id : Id.t
    ; sell : Amount.t
    ; fee : Amount.t
    ; buy : Amount.t
    ; market : Money.Rate.t
    ; spread_bps : int
    ; created_at : Time_ns.t
    ; expires_at : Time_ns.t
    }
  [@@deriving sexp_of, compare, equal]

  let create
    ~id
    ~market
    ~sell
    ~buy:buy_currency
    ?(spread_bps = 0)
    ?fee
    ~now
    ?(ttl = Time_ns.Span.of_sec 30.)
    ()
    =
    let open Or_error.Let_syntax in
    let sell_currency = Amount.currency sell in
    let%bind market =
      if Currency.equal (Money.Rate.base market) sell_currency
         && Currency.equal (Money.Rate.quote market) buy_currency
      then Ok market
      else if Currency.equal (Money.Rate.base market) buy_currency
              && Currency.equal (Money.Rate.quote market) sell_currency
      then Ok (Money.Rate.inverse market)
      else
        Or_error.error_s
          [%message
            "rate does not connect the currencies"
              (market : Money.Rate.t)
              (sell_currency : Currency.t)
              (buy_currency : Currency.t)]
    in
    let%bind () =
      if Amount.is_positive sell
      then Ok ()
      else Or_error.error_s [%message "sell amount must be positive" (sell : Amount.t)]
    in
    let%bind () =
      if spread_bps >= 0 && spread_bps < 10_000
      then Ok ()
      else Or_error.error_s [%message "spread out of range" (spread_bps : int)]
    in
    let%bind fee =
      match fee with
      | None -> Ok (Amount.zero sell_currency)
      | Some schedule -> Money.Fee.compute schedule ~side:Receivable sell
    in
    let%bind net = Amount.sub sell fee in
    let%bind () =
      if Amount.is_positive net
      then Ok ()
      else Or_error.error_s [%message "fee exceeds the amount" (sell : Amount.t) (fee : Amount.t)]
    in
    let customer_price =
      Q.(Money.Rate.price market * (one - (of_int spread_bps / of_int 10_000)))
    in
    let%bind customer_rate =
      Money.Rate.create
        ~base:sell_currency
        ~quote:buy_currency
        ~price:customer_price
        ~at:now
        ~source:"quote"
    in
    (* Round the bought amount down: the customer never receives a fraction of a minor
       unit the institution does not have. *)
    let%bind buy = Money.Rate.convert ~rounding:Down customer_rate net in
    let%map () =
      if Amount.is_positive buy
      then Ok ()
      else Or_error.error_s [%message "amount too small to buy anything" (sell : Amount.t)]
    in
    { id; sell; fee; buy; market; spread_bps; created_at = now; expires_at = Time_ns.add now ttl }
  ;;

  let effective_price t = Q.(Amount.to_q t.buy / Amount.to_q t.sell)
  let is_expired t ~now = Time_ns.( >= ) now t.expires_at
end

let execute (chart : Booking.Chart.t) balances (q : Quote.t) ~from_ ~to_ ~event_id ~now =
  let open Or_error.Let_syntax in
  let%bind () =
    if Quote.is_expired q ~now
    then Or_error.error_s [%message "quote expired" ~id:(q.id : Id.t) ~expires_at:(q.expires_at : Time_ns.Alternate_sexp.t)]
    else Ok ()
  in
  let sell_currency = Amount.currency q.sell in
  let buy_currency = Amount.currency q.buy in
  let net = Amount.sub_exn q.sell q.fee in
  let entry tag postings =
    let%bind idempotency_key =
      Ledger.Idempotency_key.of_string_or_error (event_id ^ "/" ^ tag)
    in
    Ledger.Entry.create
      ~idempotency_key
      ~kind:(Transfer tag)
      ~reference:(Id.to_string q.id)
      ~effective_at:now
      ~recorded_at:now
      postings
  in
  let%bind sell_leg =
    entry
      "swap-sell"
      [ Ledger.Posting.debit from_ net
      ; Ledger.Posting.credit (chart.fx_position sell_currency) net
      ]
  in
  let%bind fee_leg =
    if Amount.is_zero q.fee
    then Ok []
    else
      entry
        "swap-fee"
        [ Ledger.Posting.debit from_ q.fee
        ; Ledger.Posting.credit (chart.fee_income sell_currency) q.fee
        ]
      >>| List.return
  in
  let%bind buy_leg =
    entry
      "swap-buy"
      [ Ledger.Posting.debit (chart.fx_position buy_currency) q.buy
      ; Ledger.Posting.credit to_ q.buy
      ]
  in
  let entries = (sell_leg :: fee_leg) @ [ buy_leg ] in
  (* Check against the ledger now, so a short balance fails before anything is booked. *)
  let%map _ = Booking.apply balances entries in
  entries
;;
