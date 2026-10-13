open! Core

module Id =
  Opaque_id.Make
    (struct
      let module_name = "Payments_core.Standing_order.Id"
    end)
    ()

module Status = struct
  type t =
    | Active
    | Paused
    | Cancelled
    | Finished
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Stable = struct
  module V1 = struct
    type t =
      { id : Id.Stable.V1.t
      ; account : Ledger.Account_id.Stable.V1.t
      ; beneficiary : Beneficiary.Stable.V1.t
      ; rail : Rail.Stable.V1.t
      ; amount : Money.Amount.Stable.V1.t
      ; reference : string
      ; details : Transfer.Details.t option
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

include Stable.V1

let past_until t date =
  match t.until with
  | Some until -> Date.( > ) date until
  | None -> false
;;

let details_for_run ~id ~run_date (details : Transfer.Details.t option) =
  match details with
  | Some (Sepa d) ->
    let end_to_end_id = sprintf "%s-%s" (Id.to_string id) (Date.to_string run_date) in
    Or_error.map (Iso20022.Payment_id.create ~end_to_end_id ()) ~f:(fun payment_id ->
      Some (Transfer.Details.Sepa { d with payment_id }))
  | other -> Ok other
;;

let create
  ~id
  ~account
  ~beneficiary
  ~rail
  ~amount
  ?(reference = "")
  ?details
  ~schedule
  ~start
  ?until
  ()
  =
  let open Or_error.Let_syntax in
  let%bind () = Schedule.validate schedule in
  let%bind () =
    if Money.Amount.is_positive amount
    then Ok ()
    else Or_error.error_s [%message "amount must be positive" (amount : Money.Amount.t)]
  in
  let%bind () =
    if Beneficiary.usable_on beneficiary rail
    then Ok ()
    else Or_error.error_s [%message "rail cannot reach beneficiary" (rail : Rail.t)]
  in
  let first_run = Schedule.next_on_or_after schedule start in
  (* Build the first draft now, so a template the rail rejects fails here and not on the
     first run date. *)
  let%bind () =
    let%bind run_details = details_for_run ~id ~run_date:first_run details in
    Transfer.draft
      ~id:(Transfer.Id.of_string "check")
      ~rail
      ~account
      ~beneficiary
      ~amount
      ?details:run_details
      ~now:Time_ns.epoch
      ()
    |> Or_error.map ~f:ignore
  in
  let t =
    { id
    ; account
    ; beneficiary
    ; rail
    ; amount
    ; reference
    ; details
    ; schedule
    ; until
    ; next_run = first_run
    ; status = Active
    ; runs = 0
    ; last_run = None
    }
  in
  if past_until t t.next_run
  then Or_error.error_s [%message "no run before the end date" (until : Date.t option)]
  else Ok t
;;

let pause t =
  match t.status with
  | Active -> Ok { t with status = Paused }
  | Paused -> Ok t
  | Cancelled | Finished ->
    Or_error.error_s [%message "cannot pause" ~status:(t.status : Status.t)]
;;

let resume t ~today =
  match t.status with
  | Paused ->
    let next_run = Schedule.next_on_or_after t.schedule (Date.max today t.next_run) in
    if past_until t next_run
    then Ok { t with status = Finished }
    else Ok { t with status = Active; next_run }
  | Active -> Ok t
  | Cancelled | Finished ->
    Or_error.error_s [%message "cannot resume" ~status:(t.status : Status.t)]
;;

let cancel t =
  match t.status with
  | Finished -> t
  | Active | Paused | Cancelled -> { t with status = Cancelled }
;;

let is_due t ~now ~zone =
  match t.status with
  | Active -> Date.( <= ) t.next_run (Time_ns.to_date now ~zone)
  | Paused | Cancelled | Finished -> false
;;

let run t ~now ~zone =
  if not (is_due t ~now ~zone)
  then Ok None
  else (
    let today = Time_ns.to_date now ~zone in
    (* The latest schedule date that is due, so a catch-up run is labelled with the period
       it pays for now rather than the oldest missed one. *)
    let rec latest_due d =
      let next = Schedule.next_after t.schedule d in
      if Date.( <= ) next today && not (past_until t next) then latest_due next else d
    in
    let run_date = latest_due t.next_run in
    let id =
      Transfer.Id.of_string
        (sprintf "%s:%s" (Id.to_string t.id) (Date.to_string run_date))
    in
    let open Or_error.Let_syntax in
    let%bind details = details_for_run ~id:t.id ~run_date t.details in
    let%bind draft =
      Transfer.draft
        ?details
        ~id
        ~rail:t.rail
        ~account:t.account
        ~beneficiary:t.beneficiary
        ~amount:t.amount
        ~reference:t.reference
        ~now
        ()
    in
    (* RTS art. 14: SCA for the first payment of a series with the same amount and payee;
       later runs are exempt. *)
    let%bind draft =
      if t.runs >= 1
      then Transfer.exempt draft ~exemption:Recurring_same_amount_payee
      else Ok draft
    in
    let next_run = Schedule.next_after t.schedule today in
    let status : Status.t = if past_until t next_run then Finished else Active in
    return
      (Some
         ({ t with next_run; status; runs = t.runs + 1; last_run = Some run_date }, draft)))
;;

let poll t ~time_source ~zone =
  run t ~now:(Async_kernel.Time_source.now time_source) ~zone
;;
