open! Core

module Stable = struct
  module V1 = struct
    type t =
      | AB05
      | AB06
      | AB07
      | AB08
      | AB09
      | AB10
      | AC01
      | AC03
      | AC04
      | AC06
      | AG01
      | AG02
      | AM02
      | AM04
      | AM05
      | AM09
      | AM23
      | ARDT
      | BE04
      | CNOR
      | CUST
      | DNOR
      | DUPL
      | ERIN
      | FF01
      | FOCR
      | FRAD
      | LEGL
      | MD07
      | MS02
      | MS03
      | NOAS
      | NOOR
      | RC01
      | RR01
      | RR02
      | RR03
      | RR04
      | TECH
      | TM01
      | Other of string
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end

include Stable.V1

let table =
  [ AB05, "Timeout at the creditor agent"
  ; AB06, "Timeout at an instructed agent"
  ; AB07, "Offline agent"
  ; AB08, "Offline creditor agent"
  ; AB09, "Error at the creditor agent"
  ; AB10, "Error at an instructed agent"
  ; AC01, "Incorrect account number"
  ; AC03, "Wrong creditor account"
  ; AC04, "Closed account number"
  ; AC06, "Blocked account"
  ; AG01, "Transaction forbidden"
  ; AG02, "Invalid bank operation code"
  ; AM02, "Not allowed amount"
  ; AM04, "Insufficient funds"
  ; AM05, "Duplication"
  ; AM09, "Wrong amount"
  ; AM23, "Amount exceeds settlement limit"
  ; ARDT, "Already returned"
  ; BE04, "Missing creditor address"
  ; CNOR, "Creditor bank is not registered"
  ; CUST, "Requested by customer"
  ; DNOR, "Debtor bank is not registered"
  ; DUPL, "Duplicate payment"
  ; ERIN, "ERI option not supported"
  ; FF01, "Invalid file format"
  ; FOCR, "Following cancellation request"
  ; FRAD, "Fraudulent origin"
  ; LEGL, "Legal decision"
  ; MD07, "End customer deceased"
  ; MS02, "Not specified reason, customer generated"
  ; MS03, "Not specified reason, agent generated"
  ; NOAS, "No answer from customer"
  ; NOOR, "No original transaction received"
  ; RC01, "Bank identifier incorrect"
  ; RR01, "Missing debtor account or identification"
  ; RR02, "Missing debtor name or address"
  ; RR03, "Missing creditor name or address"
  ; RR04, "Regulatory reason"
  ; TECH, "Technical problem"
  ; TM01, "Received after cut-off time"
  ]
;;

let known = List.map table ~f:fst

let to_code = function
  | Other code -> code
  | t -> Sexp.to_string (sexp_of_t t)
;;

let of_code raw =
  let code = String.uppercase (String.strip raw) in
  if String.length code <> 4 || not (String.for_all code ~f:Char.is_alphanum)
  then Or_error.error_s [%message "reason code must be four alphanumerics" raw]
  else
    Ok
      (List.find known ~f:(fun t -> String.equal (to_code t) code)
       |> Option.value ~default:(Other code))
;;

let description = function
  | Other code -> "Unlisted reason " ^ code
  | t -> List.Assoc.find_exn table t ~equal
;;

module Kind = struct
  type t =
    | Reject
    | Return
    | Recall
    | Rfro
    | Answer
  [@@deriving sexp, compare, equal, enumerate]
end

let kinds : t -> Kind.t list = function
  | AB05 | AB06 | AB07 | AB08 | AB09 | AB10 | AM23 | CNOR | DNOR | FF01 | TM01 -> [ Reject ]
  | AC01 | AG02 | AM05 | ERIN | MS03 | RC01 | RR01 | RR02 | RR03 | RR04 -> [ Reject; Return ]
  | AC06 | AG01 | BE04 | MD07 | MS02 -> [ Reject; Return ]
  | AC04 -> [ Reject; Return; Answer ]
  | FOCR -> [ Return ]
  | DUPL | TECH | FRAD -> [ Recall ]
  | AC03 | AM09 -> [ Rfro ]
  | CUST -> [ Rfro; Answer ]
  | AM04 | ARDT | LEGL | NOAS | NOOR -> [ Answer ]
  | AM02 | Other _ -> []
;;

let is_customer_error = function
  | AC01 | AC04 | AC06 | AG01 | BE04 | MD07 | MS02 -> true
  | _ -> false
;;
