(* Small command line front end: validate payment details, list schedule dates and print
   the ledger postings of a sample transfer lifecycle. *)

open! Core
open Payments_core

let print_result to_sexp = function
  | Ok x -> print_s (to_sexp x)
  | Error e ->
    print_s [%message "invalid" ~_:(e : Error.t)];
    exit 1
;;

let iban =
  Command.basic
    ~summary:"Validate an IBAN and print its grouped form"
    (let%map_open.Command raw = anon ("IBAN" %: string) in
     fun () ->
       Iban.of_string raw
       |> print_result (fun i ->
         [%message
           (Iban.to_string_grouped i)
             ~country:(Iban.country i : string)
             ~sepa:(Iban.is_sepa i : bool)]))
;;

let bic =
  Command.basic
    ~summary:"Validate a BIC"
    (let%map_open.Command raw = anon ("BIC" %: string) in
     fun () -> Bic.of_string raw |> print_result Bic.sexp_of_t)
;;

let address =
  Command.basic
    ~summary:"Validate a crypto address for a network"
    (let%map_open.Command network =
       flag
         "network"
         (required (Arg_type.create Network.of_string))
         ~doc:
           (sprintf
              "NETWORK one of %s"
              (String.concat ~sep:", " (List.map Network.all ~f:Network.to_string)))
     and raw = anon ("ADDRESS" %: string) in
     fun () -> Crypto_address.create network raw |> print_result Crypto_address.sexp_of_t)
;;

let lightning =
  Command.basic
    ~summary:"Decode a Lightning invoice, LNURL or Lightning address"
    (let%map_open.Command raw = anon ("TARGET" %: string) in
     fun () ->
       Lightning.Target.of_string raw
       |> print_result (fun t ->
         [%message
           (t : Lightning.Target.t) ~pay_url:(Lightning.Target.pay_url t : string option)]))
;;

let schedule =
  Command.basic
    ~summary:"List the next run dates of a standing order schedule"
    (let%map_open.Command daily = flag "daily" no_arg ~doc:" every day"
     and weekly = flag "weekly" (optional (Arg_type.create Day_of_week.of_string)) ~doc:"DAY every week on DAY"
     and monthly = flag "monthly" (optional int) ~doc:"N every month on day N (clamped)"
     and from = flag "from" (required (Arg_type.create Date.of_string)) ~doc:"DATE first possible date"
     and count = flag "count" (optional_with_default 6 int) ~doc:"N dates to print" in
     fun () ->
       let t : Schedule.t =
         match daily, weekly, monthly with
         | true, None, None -> Daily
         | false, Some d, None -> Weekly d
         | false, None, Some day -> Monthly { day }
         | _ -> failwith "give exactly one of -daily, -weekly, -monthly"
       in
       Schedule.validate t |> Or_error.ok_exn;
       let rec go date n =
         if n > 0
         then (
           print_endline (Date.to_string date);
           go (Schedule.next_after t date) (n - 1))
       in
       go (Schedule.next_on_or_after t from) count)
;;

let demo =
  Command.basic
    ~summary:"Print the postings of a sample outgoing SEPA transfer"
    (let%map_open.Command amount =
       flag "amount" (optional_with_default "25.00" string) ~doc:"EUR amount (default 25.00)"
     and fee = flag "fee" (optional_with_default "0.50" string) ~doc:"EUR fee (default 0.50)"
     and fail = flag "fail" no_arg ~doc:" fail after submission instead of settling" in
     fun () ->
       let now = Time_ns.epoch in
       let eur s = Money.Amount.of_decimal_string Money.Currency.eur s |> Or_error.ok_exn in
       let customer = Ledger.Account_id.of_string "customer:demo/eur" in
       let chart = Booking.Chart.default in
       let open_ b a =
         Ledger.Balances.apply b { seq = Ledger.Balances.next_seq b; event = Account_opened a }
         |> Or_error.ok_exn
       in
       let b =
         List.fold
           (Ledger.Account.create customer Liability
            :: Booking.Chart.accounts chart ~rails:[ Sepa_credit ] ~currencies:[ Money.Currency.eur ])
           ~init:Ledger.Balances.empty
           ~f:open_
       in
       let print entries =
         List.iter entries ~f:(fun (e : Ledger.Entry.t) ->
           printf "%s\n" (Ledger.Idempotency_key.to_string e.idempotency_key);
           List.iter e.postings ~f:(fun p ->
             printf
               "  %-6s %-32s %s\n"
               (Sexp.to_string [%sexp (p.side : Ledger.Side.t)])
               (Ledger.Account_id.to_string p.account)
               (Money.Amount.to_string p.amount)))
       in
       let step (b, t) to_ event_id =
         let entries = Booking.transition chart b t ~to_ ~event_id ~at:now |> Or_error.ok_exn in
         printf "-> %s\n" (Transfer.Status.name to_);
         print entries;
         ( Booking.apply b entries |> Or_error.ok_exn
         , Transfer.transition t ~to_ ~at:now |> Or_error.ok_exn )
       in
       let deposit =
         Transfer.incoming
           ~id:(Transfer.Id.of_string "deposit")
           ~rail:Sepa_credit
           ~account:customer
           ~from:(External { name = "Payer"; detail = "demo" })
           ~amount:(eur "100.00")
           ~status:Settled
           ~now
           ()
         |> Or_error.ok_exn
       in
       let entries = Booking.created chart b deposit ~event_id:"evt-deposit" ~at:now |> Or_error.ok_exn in
       print entries;
       let b = Booking.apply b entries |> Or_error.ok_exn in
       let beneficiary =
         Beneficiary.create
           ~id:(Beneficiary.Id.of_string "demo")
           ~owner:"demo"
           ~name:"Demo Payee"
           ~rail:Sepa_credit
           ~now
           "DE89370400440532013000"
         |> Or_error.ok_exn
       in
       let t =
         Transfer.draft
           ~id:(Transfer.Id.of_string "demo-transfer")
           ~rail:Sepa_credit
           ~account:customer
           ~beneficiary
           ~amount:(eur amount)
           ~fee:(eur fee)
           ~now
           ()
         |> Or_error.ok_exn
       in
       let s = step (b, t) Submitted "evt-1" in
       let b, _ =
         if fail
         then step s (Failed Beneficiary_rejected) "evt-2"
         else step (step s Pending "evt-2") Settled "evt-3"
       in
       printf
         "available %s\n"
         (Money.Amount.to_string (Ledger.Balances.available b customer Money.Currency.eur));
       print_string (Ledger.Trial_balance.to_string_hum (Ledger.Trial_balance.of_balances b)))
;;

let () =
  Command.group
    ~summary:"Payment rails, beneficiaries and transfer booking"
    [ "iban", iban
    ; "bic", bic
    ; "address", address
    ; "lightning", lightning
    ; "schedule", schedule
    ; "demo", demo
    ]
  |> Command_unix.run
;;
