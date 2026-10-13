open! Core
open Payments_core
open Helpers

let zone = Time_float.Zone.utc
let day s = Time_ns.of_string_with_utc_offset (s ^ " 08:00:00Z")

let order ?until () =
  Standing_order.create
    ~id:(Standing_order.Id.of_string "so-1")
    ~account:customer
    ~beneficiary:(beneficiary ())
    ~rail:Sepa_credit
    ~amount:(eur "750.00")
    ~reference:"rent"
    ~details:(sepa_details ())
    ~schedule:(Monthly { day = 31 })
    ~start:(Date.of_string "2026-10-13")
    ?until
    ()
  |> Or_error.ok_exn
;;

let tick t now =
  match Standing_order.run t ~now:(day now) ~zone |> Or_error.ok_exn with
  | None ->
    printf "%s  not due (next %s, %s)\n" now (Date.to_string t.next_run)
      (Sexp.to_string [%sexp (t.status : Standing_order.Status.t)]);
    t
  | Some (t, draft) ->
    let e2e =
      match draft.details with
      | Sepa d -> d.payment_id.end_to_end_id
      | _ -> "-"
    in
    printf
      "%s  draft %s e2e=%s %s -> next %s (%s)\n"
      now
      (Transfer.Id.to_string draft.id)
      e2e
      (Money.Amount.to_string draft.amount)
      (Date.to_string t.next_run)
      (Sexp.to_string [%sexp (t.status : Standing_order.Status.t)]);
    t
;;

let%expect_test "month-end rent with a pause and an end date" =
  let t = order ~until:(Date.of_string "2027-02-28") () in
  let t = tick t "2026-10-30" in
  let t = tick t "2026-10-31" in
  let t = tick t "2026-10-31" in
  let t = Standing_order.pause t |> Or_error.ok_exn in
  let t = tick t "2026-11-30" in
  let t = Standing_order.resume t ~today:(Date.of_string "2026-12-02") |> Or_error.ok_exn in
  let t = tick t "2026-12-31" in
  (* down for two months: one draft, not three *)
  let t = tick t "2027-02-28" in
  let t = tick t "2027-03-31" in
  print_s [%message (t.runs : int) (t.last_run : Date.t option)];
  [%expect {|
    2026-10-30  not due (next 2026-10-31, Active)
    2026-10-31  draft so-1:2026-10-31 e2e=so-1-2026-10-31 750.00 EUR -> next 2026-11-30 (Active)
    2026-10-31  not due (next 2026-11-30, Active)
    2026-11-30  not due (next 2026-11-30, Paused)
    2026-12-31  draft so-1:2026-12-31 e2e=so-1-2026-12-31 750.00 EUR -> next 2027-01-31 (Active)
    2027-02-28  draft so-1:2027-02-28 e2e=so-1-2027-02-28 750.00 EUR -> next 2027-03-31 (Finished)
    2027-03-31  not due (next 2027-03-31, Finished)
    ((t.runs 3) (t.last_run (2027-02-28)))
    |}]
;;

let%expect_test "cancel and validation" =
  let t = Standing_order.cancel (order ()) in
  let _ = tick t "2026-10-31" in
  print_s [%message (Standing_order.resume t ~today:(Date.of_string "2026-11-01") : Standing_order.t Or_error.t)];
  let bad =
    Standing_order.create
      ~id:(Standing_order.Id.of_string "so-2")
      ~account:customer
      ~beneficiary:(beneficiary ())
      ~rail:Sepa_credit
      ~amount:(eur "1.00")
      ~details:(sepa_details ())
      ~schedule:(Monthly { day = 32 })
      ~start:(Date.of_string "2026-10-13")
      ()
  in
  print_s [%message (Result.is_error bad : bool)];
  [%expect {|
    2026-10-31  not due (next 2026-10-31, Cancelled)
    ("Standing_order.resume t ~today:(Date.of_string \"2026-11-01\")"
     (Error ("cannot resume" (status Cancelled))))
    ("Result.is_error bad" true)
    |}]
;;

let%expect_test "poll reads the time source" =
  let time_source =
    Async_kernel.Time_source.create ~now:(day "2026-10-31") () |> Async_kernel.Time_source.read_only
  in
  let r = Standing_order.poll (order ()) ~time_source ~zone |> Or_error.ok_exn in
  print_s [%message (Option.map r ~f:(fun (_, d) -> d.id) : Transfer.Id.t option)];
  [%expect {| ("Option.map r ~f:(fun (_, d) -> d.id)" (so-1:2026-10-31)) |}]
;;
