open! Core

module Scheme = struct
  type t =
    | Sct
    | Sct_inst
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]

  let of_rail : Rail.t -> t option = function
    | Sepa_credit -> Some Sct
    | Sepa_instant -> Some Sct_inst
    | Onchain _ | Lightning | Intraledger | Card_scheme -> None
  ;;
end

module Details = struct
  module Stable = struct
    module V1 = struct
      type t =
        { payment_id : Iso20022.Payment_id.t
        ; debtor : Iso20022.Party.t
        ; creditor : Iso20022.Party.t
        ; remittance : Iso20022.Remittance.t
        ; charge_bearer : Iso20022.Charge_bearer.t
        ; purpose : string option
        ; requested_execution_date : Date.Stable.V1.t option
        ; acceptance_time : Utc_time.Stable.V1.t option
        ; value_date : Date.Stable.V1.t option
        ; booking_date : Date.Stable.V1.t option
        ; last_status : Iso20022.Transaction_status.t option
        }
      [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  include Stable.V1

  let create
    ~payment_id
    ~debtor
    ~creditor
    ?(remittance = Iso20022.Remittance.None_given)
    ?purpose
    ?requested_execution_date
    ()
    =
    { payment_id
    ; debtor
    ; creditor
    ; remittance
    ; charge_bearer = SLEV
    ; purpose
    ; requested_execution_date
    ; acceptance_time = None
    ; value_date = None
    ; booking_date = None
    ; last_status = None
    }
  ;;
end

(* The EPC removed the 100,000 EUR SCT Inst ceiling from the rulebook in October 2025
   (the Instant Payments Regulation lets customers set their own limits instead). *)
let inst_max_amount = None

let validate (scheme : Scheme.t) (d : Details.t) ~amount =
  let open Or_error.Let_syntax in
  let%bind () =
    if Money.Currency.equal (Money.Amount.currency amount) Money.Currency.eur
    then Ok ()
    else Or_error.error_s [%message "SEPA transfers are in EUR" (amount : Money.Amount.t)]
  in
  let%bind () =
    match d.charge_bearer with
    | SLEV -> Ok ()
    | other ->
      Or_error.error_s
        [%message "SEPA requires charge bearer SLEV" ~got:(other : Iso20022.Charge_bearer.t)]
  in
  let%bind () = Iso20022.Party.validate d.debtor in
  let%bind () = Iso20022.Party.validate d.creditor in
  let%bind () =
    List.map [ "debtor", d.debtor; "creditor", d.creditor ] ~f:(fun (side, p) ->
      if Iban.is_sepa p.iban
      then Ok ()
      else Or_error.error_s [%message "IBAN outside SEPA" side (Iban.country p.iban)])
    |> Or_error.all_unit
  in
  let%bind () =
    match d.purpose with
    | Some p when String.length p <> 4 || not (String.for_all p ~f:Char.is_alphanum) ->
      Or_error.error_s [%message "purpose must be a four-character code" p]
    | _ -> Ok ()
  in
  match scheme, inst_max_amount with
  | Sct_inst, Some max ->
    (match Money.Amount.compare_value amount max with
     | Ok c when c > 0 ->
       Or_error.error_s
         [%message "over the SCT Inst maximum" (amount : Money.Amount.t) (max : Money.Amount.t)]
     | _ -> Ok ())
  | _ -> Ok ()
;;

module Use = struct
  type t =
    | Reject
    | Return
    | Recall
    | Recall_refusal
  [@@deriving sexp, compare, equal, enumerate]
end

let reasons (scheme : Scheme.t) (use : Use.t) : Iso_reason.t list =
  let common_reject_return : Iso_reason.t list =
    [ AC01; AC04; AC06; AG01; AG02; AM05; BE04; MD07; MS02; MS03; RC01; RR01; RR02; RR03; RR04 ]
  in
  match use with
  | Reject ->
    let inst_only : Iso_reason.t list =
      match scheme with
      | Sct_inst -> [ AB05; AB06; AB07; AB08; AB09; AB10; AM23 ]
      | Sct -> []
    in
    common_reject_return @ Iso_reason.[ CNOR; DNOR; ERIN; FF01; TM01 ] @ inst_only
  | Return -> common_reject_return @ Iso_reason.[ ERIN; FOCR ]
  | Recall -> [ DUPL; TECH; FRAD; AC03; AM09; CUST ]
  | Recall_refusal -> [ NOAS; NOOR; ARDT; AC04; AM04; LEGL; CUST ]
;;

let check_reason scheme use reason =
  if List.mem (reasons scheme use) reason ~equal:Iso_reason.equal
  then Ok ()
  else
    Or_error.error_s
      [%message
        "reason code not allowed by the rulebook"
          (scheme : Scheme.t)
          (use : Use.t)
          ~reason:(Iso_reason.to_code reason)]
;;

module Inst = struct
  let max_execution_time = Time_ns.Span.of_sec 10.

  (* The originator bank's time-out deadline is the 10 second target plus a margin for
     the confirmation to travel back. *)
  let timeout = Time_ns.Span.of_sec 20.

  let timed_out ~acceptance_time ~now =
    Time_ns.Span.( > ) (Time_ns.diff now acceptance_time) timeout
  ;;
end

module Deadline = struct
  let return ~settled_on = Target_calendar.add_business_days settled_on 3

  let recall ~settled_on (reason : Iso_reason.t) =
    match reason with
    | FRAD | AC03 | AM09 | CUST -> Date.add_months settled_on 13
    | _ -> Target_calendar.add_business_days settled_on 10
  ;;

  let recall_answer ~received_on = Target_calendar.add_business_days received_on 15
end
