open! Core

module Id =
  Opaque_id.Make
    (struct
      let module_name = "Payments_core.Transfer.Id"
    end)
    ()

module Direction = struct
  type t =
    | In
    | Out
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
end

module Failure_reason = struct
  type t =
    | Insufficient_funds
    | Sca_failed
    | Beneficiary_rejected
    | Expired
    | Provider_rejected of string
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Status = struct
  type t =
    | Draft
    | Awaiting_sca
    | Submitted
    | Pending
    | Settled
    | Failed of Failure_reason.t
    | Returned
    | Cancelled
  [@@deriving bin_io, sexp, compare, equal, hash]

  let name = function
    | Draft -> "Draft"
    | Awaiting_sca -> "Awaiting_sca"
    | Submitted -> "Submitted"
    | Pending -> "Pending"
    | Settled -> "Settled"
    | Failed _ -> "Failed"
    | Returned -> "Returned"
    | Cancelled -> "Cancelled"
  ;;

  let is_final = function
    | Failed _ | Returned | Cancelled -> true
    | Draft | Awaiting_sca | Submitted | Pending | Settled -> false
  ;;
end

let allowed_transitions =
  [ "Draft", [ "Awaiting_sca"; "Submitted"; "Cancelled"; "Failed" ]
  ; "Awaiting_sca", [ "Submitted"; "Cancelled"; "Failed" ]
  ; "Submitted", [ "Pending"; "Settled"; "Failed" ]
  ; "Pending", [ "Settled"; "Failed" ]
  ; "Settled", [ "Returned" ]
  ; "Failed", []
  ; "Returned", []
  ; "Cancelled", []
  ]
;;

module Counterparty = struct
  type t =
    | Beneficiary of Beneficiary.Stable.V1.t
    | External of
        { name : string
        ; detail : string
        }
  [@@deriving bin_io, sexp, compare, equal]
end

module Stable = struct
  module V1 = struct
    type t =
      { id : Id.Stable.V1.t
      ; direction : Direction.t
      ; rail : Rail.Stable.V1.t
      ; account : Ledger.Account_id.Stable.V1.t
      ; counterparty : Counterparty.t
      ; amount : Money.Amount.Stable.V1.t
      ; fee : Money.Amount.Stable.V1.t
      ; reference : string
      ; status : Status.t
      ; history : (Status.t * Utc_time.Stable.V1.t) list
      ; sca : Sca.Id.Stable.V1.t option
      ; provider_ref : string option
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

include Stable.V1

let check_amounts ~amount ~fee =
  let currency = Money.Amount.currency amount in
  if not (Money.Amount.is_positive amount)
  then Or_error.error_s [%message "amount must be positive" (amount : Money.Amount.t)]
  else if Money.Amount.is_negative fee
  then Or_error.error_s [%message "fee must not be negative" (fee : Money.Amount.t)]
  else if not (Money.Currency.equal currency (Money.Amount.currency fee))
  then
    Or_error.error_s
      [%message "fee currency differs" (amount : Money.Amount.t) (fee : Money.Amount.t)]
  else Ok ()
;;

let draft ~id ~rail ~account ~beneficiary ~amount ?fee ?(reference = "") ~now () =
  let open Or_error.Let_syntax in
  let fee = Option.value fee ~default:(Money.Amount.zero (Money.Amount.currency amount)) in
  let%bind () = check_amounts ~amount ~fee in
  let%map () =
    if Beneficiary.usable_on beneficiary rail
    then Ok ()
    else
      Or_error.error_s
        [%message
          "rail cannot reach beneficiary"
            (rail : Rail.t)
            ~beneficiary:(beneficiary.id : Beneficiary.Id.t)]
  in
  { id
  ; direction = Out
  ; rail
  ; account
  ; counterparty = Beneficiary beneficiary
  ; amount
  ; fee
  ; reference
  ; status = Draft
  ; history = [ Draft, now ]
  ; sca = None
  ; provider_ref = None
  }
;;

let incoming
  ~id
  ~rail
  ~account
  ~from
  ~amount
  ?fee
  ?(reference = "")
  ?provider_ref
  ~(status : Status.t)
  ~now
  ()
  =
  let open Or_error.Let_syntax in
  let fee = Option.value fee ~default:(Money.Amount.zero (Money.Amount.currency amount)) in
  let%bind () = check_amounts ~amount ~fee in
  let%map () =
    match status with
    | Pending | Settled -> Ok ()
    | _ -> Or_error.error_s [%message "incoming transfers start Pending or Settled" (status : Status.t)]
  in
  { id
  ; direction = In
  ; rail
  ; account
  ; counterparty = from
  ; amount
  ; fee
  ; reference
  ; status
  ; history = [ status, now ]
  ; sca = None
  ; provider_ref
  }
;;

let can_transition t (to_ : Status.t) =
  let by_table =
    List.Assoc.find allowed_transitions ~equal:String.equal (Status.name t.status)
    |> Option.value ~default:[]
    |> fun targets -> List.mem targets (Status.name to_) ~equal:String.equal
  in
  match to_ with
  | Returned -> by_table && Rail.is_returnable t.rail
  | _ -> by_table
;;

let transition t ~to_ ~at =
  if can_transition t to_
  then Ok { t with status = to_; history = (to_, at) :: t.history }
  else
    Or_error.error_s
      [%message
        "transition not allowed"
          ~transfer:(t.id : Id.t)
          ~from:(t.status : Status.t)
          (to_ : Status.t)
          ~rail:(t.rail : Rail.t)]
;;

let require_sca t ~challenge ~at =
  Or_error.map (transition t ~to_:Awaiting_sca ~at) ~f:(fun t -> { t with sca = Some challenge })
;;

let submit t ?provider_ref ~at () =
  Or_error.map (transition t ~to_:Submitted ~at) ~f:(fun t ->
    { t with provider_ref = Option.first_some provider_ref t.provider_ref })
;;

let is_terminal t =
  Status.is_final t.status
  || (Status.equal t.status Settled && not (Rail.is_returnable t.rail))
;;

let total_debit t = Money.Amount.add_exn t.amount t.fee

let entered_at t status =
  List.find_map t.history ~f:(fun (s, at) -> Option.some_if (Status.equal s status) at)
;;
