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
    | Expired
    | Provider_error of string
  [@@deriving bin_io, sexp, compare, equal, hash]
end

module Status = struct
  type t =
    | Draft
    | Awaiting_sca
    | Submitted
    | Pending
    | Settled
    | Rejected of Iso_reason.Stable.V1.t
    | Failed of Failure_reason.t
    | Returned of Iso_reason.Stable.V1.t
    | Recall_requested of Iso_reason.Stable.V1.t
    | Recall_refused of Iso_reason.Stable.V1.t
    | Recalled
    | Cancelled
    | Timed_out
    | Reorged
  [@@deriving bin_io, sexp, compare, equal, hash]

  let name = function
    | Draft -> "Draft"
    | Awaiting_sca -> "Awaiting_sca"
    | Submitted -> "Submitted"
    | Pending -> "Pending"
    | Settled -> "Settled"
    | Rejected _ -> "Rejected"
    | Failed _ -> "Failed"
    | Returned _ -> "Returned"
    | Recall_requested _ -> "Recall_requested"
    | Recall_refused _ -> "Recall_refused"
    | Recalled -> "Recalled"
    | Cancelled -> "Cancelled"
    | Timed_out -> "Timed_out"
    | Reorged -> "Reorged"
  ;;

  let is_final = function
    | Rejected _ | Failed _ | Returned _ | Recall_refused _ | Recalled | Cancelled -> true
    | Draft
    | Awaiting_sca
    | Submitted
    | Pending
    | Settled
    | Recall_requested _
    | Timed_out
    | Reorged -> false
  ;;
end

let allowed_transitions =
  [ "Draft", [ "Awaiting_sca"; "Submitted"; "Cancelled"; "Failed" ]
  ; "Awaiting_sca", [ "Submitted"; "Cancelled"; "Failed" ]
  ; "Submitted", [ "Pending"; "Settled"; "Rejected"; "Failed"; "Timed_out" ]
  ; "Pending", [ "Settled"; "Rejected"; "Failed"; "Timed_out"; "Reorged" ]
  ; "Settled", [ "Returned"; "Recall_requested"; "Reorged" ]
  ; "Timed_out", [ "Settled"; "Rejected" ]
  ; "Reorged", [ "Pending" ]
    (* A return can cross a recall in flight; it then answers it. *)
  ; "Recall_requested", [ "Recalled"; "Recall_refused"; "Returned" ]
  ; "Rejected", []
  ; "Failed", []
  ; "Returned", []
  ; "Recall_refused", []
  ; "Recalled", []
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

module Details = struct
  type t =
    | Sepa of Sepa.Details.Stable.V1.t
    | Onchain of
        { travel_rule : Travel_rule.Stable.V1.t option
        ; tx_hash : string option
        ; block_hash : string option
        ; confirmations : int
        ; finalized : bool
        }
    | Lightning of
        { payment_hash : string option
        ; preimage : string option
        }
    | Intraledger
    | Card
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
      ; details : Details.t
      ; status : Status.t
      ; history : (Status.t * Utc_time.Stable.V1.t) list
      ; authorization : Sca.Authorization.t option
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

(* Details must match the rail; [creditor_iban] is the beneficiary's IBAN for outgoing
   SEPA transfers. *)
let check_details
  ~(direction : Direction.t)
  ~rail
  ~amount
  ?creditor_iban
  ?eur_value
  details
  =
  let open Or_error.Let_syntax in
  match Sepa.Scheme.of_rail rail, (details : Details.t option), (rail : Rail.t) with
  | Some scheme, Some (Sepa d), _ ->
    let%bind () = Sepa.validate scheme d ~amount in
    let%map () =
      match creditor_iban with
      | Some iban when not (Iban.equal iban d.creditor.iban) ->
        Or_error.error_s
          [%message "creditor IBAN differs from the beneficiary" (iban : Iban.t)]
      | _ -> Ok ()
    in
    Details.Sepa d
  | Some _, _, _ ->
    Or_error.error_s [%message "SEPA rails need SEPA details" (rail : Rail.t)]
  | None, Some (Onchain { travel_rule = Some tr; _ } as d), Onchain _ ->
    let%map () = Travel_rule.check tr ~eur_value in
    d
  | None, Some (Onchain { travel_rule = None; _ }), Onchain _ | None, None, Onchain _ ->
    (match direction with
     | Out -> Or_error.error_string "on-chain transfers need travel-rule data"
     | In ->
       Ok
         (Details.Onchain
            { travel_rule = None
            ; tx_hash = None
            ; block_hash = None
            ; confirmations = 0
            ; finalized = false
            }))
  | None, Some (Lightning _ as d), Lightning -> Ok d
  | None, None, Lightning ->
    Ok (Details.Lightning { payment_hash = None; preimage = None })
  | None, (Some Intraledger | None), Intraledger -> Ok Details.Intraledger
  | None, (Some Card | None), Card_scheme -> Ok Details.Card
  | None, _, _ ->
    Or_error.error_s [%message "details do not match the rail" (rail : Rail.t)]
;;

let draft
  ~id
  ~rail
  ~account
  ~(beneficiary : Beneficiary.t)
  ~amount
  ?fee
  ?(reference = "")
  ?details
  ?eur_value
  ~now
  ()
  =
  let open Or_error.Let_syntax in
  let fee =
    Option.value fee ~default:(Money.Amount.zero (Money.Amount.currency amount))
  in
  let%bind () = check_amounts ~amount ~fee in
  let%bind () =
    if Beneficiary.usable_on beneficiary rail
    then Ok ()
    else
      Or_error.error_s
        [%message
          "rail cannot reach beneficiary"
            (rail : Rail.t)
            ~beneficiary:(beneficiary.id : Beneficiary.Id.t)]
  in
  let creditor_iban =
    match beneficiary.destination with
    | Bank_account { iban; _ } -> Some iban
    | _ -> None
  in
  let details =
    (* A Lightning invoice already names the payment hash. *)
    match details, beneficiary.destination with
    | None, Lightning (Invoice i) ->
      Some (Details.Lightning { payment_hash = Some i.payment_hash; preimage = None })
    | d, _ -> d
  in
  let%map details =
    check_details ~direction:Out ~rail ~amount ?creditor_iban ?eur_value details
  in
  { id
  ; direction = Out
  ; rail
  ; account
  ; counterparty = Beneficiary beneficiary
  ; amount
  ; fee
  ; reference
  ; details
  ; status = Draft
  ; history = [ Draft, now ]
  ; authorization = None
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
  ?details
  ?provider_ref
  ~(status : Status.t)
  ~now
  ()
  =
  let open Or_error.Let_syntax in
  let fee =
    Option.value fee ~default:(Money.Amount.zero (Money.Amount.currency amount))
  in
  let%bind () = check_amounts ~amount ~fee in
  let%bind () =
    (* The fee is booked out of the credited amount; a larger fee would overdraw. *)
    match Money.Amount.compare_value fee amount with
    | Ok c when c > 0 ->
      Or_error.error_s
        [%message
          "fee exceeds the incoming amount"
            (amount : Money.Amount.t)
            (fee : Money.Amount.t)]
    | _ -> Ok ()
  in
  let%bind () =
    match status with
    | Pending | Settled -> Ok ()
    | _ ->
      Or_error.error_s
        [%message "incoming transfers start Pending or Settled" (status : Status.t)]
  in
  let%map details = check_details ~direction:In ~rail ~amount details in
  { id
  ; direction = In
  ; rail
  ; account
  ; counterparty = from
  ; amount
  ; fee
  ; reference
  ; details
  ; status
  ; history = [ status, now ]
  ; authorization = None
  ; provider_ref
  }
;;

let by_table t (to_ : Status.t) =
  List.Assoc.find allowed_transitions ~equal:String.equal (Status.name t.status)
  |> Option.value ~default:[]
  |> fun targets -> List.mem targets (Status.name to_) ~equal:String.equal
;;

let can_transition t (to_ : Status.t) =
  by_table t to_
  &&
  match to_, t.rail with
  (* SCT Inst has no returns after acceptance, only the FOCR answer to a recall. *)
  | Returned _, Sepa_instant -> Status.name t.status |> String.equal "Recall_requested"
  | (Returned _ | Recall_requested _ | Recall_refused _ | Recalled), rail ->
    Rail.is_returnable rail
  | Timed_out, rail -> Rail.equal rail Sepa_instant
  (* An outgoing payment is settled at our confirmation threshold; a reorg after that is a
     chain incident for operations, not a state that could capture the hold twice. *)
  | Reorged, Onchain _ ->
    (match t.direction, t.status with
     | Out, Settled -> false
     | _ -> true)
  | Reorged, _ -> false
  | _ -> true
;;

(* On SEPA rails the reason code must be one the rulebook lists for this use. *)
let check_reason t (to_ : Status.t) =
  match Sepa.Scheme.of_rail t.rail with
  | None -> Ok ()
  | Some scheme ->
    (match to_ with
     | Rejected r -> Sepa.check_reason scheme Reject r
     | Returned r -> Sepa.check_reason scheme Return r
     | Recall_requested r ->
       (match Sepa.check_reason scheme Recall r with
        | Ok () -> Ok ()
        | Error _ -> Sepa.check_reason scheme Rfro r)
     | Recall_refused r -> Sepa.check_reason scheme Recall_refusal r
     | _ -> Ok ())
;;

let transition t ~to_ ~at =
  if not (can_transition t to_)
  then
    Or_error.error_s
      [%message
        "transition not allowed"
          ~transfer:(t.id : Id.t)
          ~from:(t.status : Status.t)
          (to_ : Status.t)
          ~rail:(t.rail : Rail.t)]
  else
    Or_error.map (check_reason t to_) ~f:(fun () ->
      { t with status = to_; history = (to_, at) :: t.history })
;;

let require_sca t ~challenge ~at =
  Or_error.map (transition t ~to_:Awaiting_sca ~at) ~f:(fun t ->
    { t with authorization = Some (Sca challenge) })
;;

let exempt t ~exemption =
  match t.status with
  | Draft -> Ok { t with authorization = Some (Exempt exemption) }
  | status -> Or_error.error_s [%message "exemptions apply to drafts" (status : Status.t)]
;;

let map_sepa t ~f =
  match t.details with
  | Sepa d -> { t with details = Sepa (f d) }
  | _ -> t
;;

let submit t ?provider_ref ~at () =
  let%bind.Or_error () =
    match t.direction, t.authorization with
    | Out, None ->
      Or_error.error_string "outgoing transfer needs SCA or a recorded exemption"
    | _ -> Ok ()
  in
  Or_error.map (transition t ~to_:Submitted ~at) ~f:(fun t ->
    { t with provider_ref = Option.first_some provider_ref t.provider_ref }
    |> map_sepa ~f:(fun d -> { d with acceptance_time = Some at }))
;;

let apply_status_report
  t
  (code : Iso20022.Transaction_status.t)
  ?reason
  ?value_date
  ~at
  ()
  =
  let open Or_error.Let_syntax in
  let%bind (target : Status.t option) =
    match code, t.status with
    (* SCT Inst confirms positively with ACCP. *)
    | ACCP, (Submitted | Pending | Timed_out) when Rail.equal t.rail Sepa_instant ->
      Ok (Some Status.Settled)
    | (RCVD | ACTC | ACCP), Submitted -> Ok None
    | (ACSP | ACWC | PDNG), Pending -> Ok None
    | (ACSP | ACWC | PDNG), _ -> Ok (Some Status.Pending)
    | (ACSC | ACCC), Settled -> Ok None
    | (ACSC | ACCC), _ -> Ok (Some Status.Settled)
    | RJCT, _ ->
      (match reason with
       | Some r -> Ok (Some (Status.Rejected r))
       | None -> Or_error.error_string "RJCT needs a reason code")
    | CANC, _ -> Ok (Some Status.Cancelled)
    | (RCVD | ACTC | ACCP), _ ->
      Or_error.error_s
        [%message
          "status report out of order"
            (code : Iso20022.Transaction_status.t)
            ~status:(t.status : Status.t)]
  in
  let%map t =
    match target with
    | None -> Ok t
    | Some to_ -> transition t ~to_ ~at
  in
  map_sepa t ~f:(fun d ->
    { d with
      last_status = Some code
    ; value_date = Option.first_some value_date d.value_date
    })
;;

let check_inst_timeout t ~now =
  match t.rail, t.status, t.details with
  | Sepa_instant, Submitted, Sepa { acceptance_time = Some acceptance_time; _ }
    when Sepa.Inst.timed_out ~acceptance_time ~now ->
    transition t ~to_:Timed_out ~at:now |> Result.ok
  | _ -> None
;;

let entered_at t name =
  List.rev t.history
  |> List.find_map ~f:(fun (s, at) ->
    Option.some_if (String.equal (Status.name s) name) at)
;;

let request_recall t ~reason ~at =
  let open Or_error.Let_syntax in
  let%bind () =
    match Sepa.Scheme.of_rail t.rail, entered_at t "Settled" with
    | Some _, Some settled_at ->
      let settled_on = Time_ns.to_date settled_at ~zone:Time_float.Zone.utc in
      let deadline = Sepa.Deadline.recall ~settled_on reason in
      if Date.( <= ) (Time_ns.to_date at ~zone:Time_float.Zone.utc) deadline
      then Ok ()
      else Or_error.error_s [%message "recall deadline passed" (deadline : Date.t)]
    | _ -> Ok ()
  in
  transition t ~to_:(Recall_requested reason) ~at
;;

let settle_lightning t ~preimage ~at =
  match t.details with
  | Lightning { payment_hash = Some hash; _ } ->
    let hex s = String.concat_map s ~f:(fun c -> sprintf "%02x" (Char.to_int c)) in
    let bytes =
      Option.try_with (fun () ->
        String.init
          (String.length preimage / 2)
          ~f:(fun i ->
            Char.of_int_exn
              (Int.of_string ("0x" ^ String.sub preimage ~pos:(2 * i) ~len:2))))
    in
    (match bytes with
     | Some b
       when String.length preimage = 64
            && String.equal
                 (hex (Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) b))
                 hash ->
       Or_error.map (transition t ~to_:Settled ~at) ~f:(fun t ->
         { t with
           details =
             Lightning
               { payment_hash = Some hash; preimage = Some (String.lowercase preimage) }
         })
     | _ -> Or_error.error_s [%message "preimage does not match the payment hash" hash])
  | _ -> Or_error.error_string "no Lightning payment hash to check the preimage against"
;;

let record_confirmations
  t
  ~tx_hash
  ~block_hash
  ~confirmations
  ~finalized
  ?eur_value
  ~at
  ()
  =
  match t.details, t.rail with
  | Onchain d, Onchain network ->
    let t =
      { t with
        details =
          Onchain
            { d with
              tx_hash = Some tx_hash
            ; block_hash = Some block_hash
            ; confirmations
            ; finalized
            }
      }
    in
    let policy = Confirmation_policy.required network ~eur_value in
    (match t.direction, t.status with
     | In, Pending when Confirmation_policy.is_met policy ~confirmations ~finalized ->
       transition t ~to_:Settled ~at
     | _ -> Ok t)
  | _ -> Or_error.error_string "not an on-chain transfer"
;;

let is_terminal t =
  Status.is_final t.status
  || (Status.equal t.status Settled && not (Rail.is_returnable t.rail))
;;

let total_debit t = Money.Amount.add_exn t.amount t.fee
