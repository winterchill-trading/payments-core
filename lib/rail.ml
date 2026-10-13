open! Core

module Stable = struct
  module V1 = struct
    type t =
      | Sepa_credit
      | Sepa_instant
      | Onchain of Network.Stable.V1.t
      | Lightning
      | Intraledger
      | Card_scheme
    [@@deriving bin_io, sexp, compare, equal, hash, quickcheck]
  end
end

include Stable.V1

let all =
  [ Sepa_credit; Sepa_instant ]
  @ List.map Network.all ~f:(fun n -> Onchain n)
  @ [ Lightning; Intraledger; Card_scheme ]
;;

let to_string = function
  | Sepa_credit -> "sepa_credit"
  | Sepa_instant -> "sepa_instant"
  | Onchain n -> "onchain:" ^ Network.to_string n
  | Lightning -> "lightning"
  | Intraledger -> "intraledger"
  | Card_scheme -> "card_scheme"
;;

let of_string s =
  match String.chop_prefix s ~prefix:"onchain:" with
  | Some n -> Onchain (Network.of_string n)
  | None ->
    (match List.find all ~f:(fun t -> String.equal (to_string t) s) with
     | Some t -> t
     | None -> raise_s [%message "unknown rail" s])
;;

let is_instant = function
  | Sepa_instant | Lightning | Intraledger | Card_scheme -> true
  | Sepa_credit | Onchain _ -> false
;;

let is_returnable = function
  | Sepa_credit | Sepa_instant | Card_scheme -> true
  | Onchain _ | Lightning | Intraledger -> false
;;

let expected_settlement = function
  (* The SCT rulebook settles on D+1; two days covers a submission after cut-off. *)
  | Sepa_credit -> Time_ns.Span.of_day 2.
  (* The SCT Inst rulebook targets 10 seconds with a 20 second hard limit. *)
  | Sepa_instant -> Time_ns.Span.of_sec 20.
  (* Six Bitcoin confirmations take about an hour; fee spikes can take much longer. *)
  | Onchain (Bitcoin | Bitcoin_testnet) -> Time_ns.Span.of_hr 2.
  | Onchain (Ethereum | Polygon | Tron | Solana) -> Time_ns.Span.of_min 10.
  | Lightning -> Time_ns.Span.of_min 1.
  | Intraledger -> Time_ns.Span.of_sec 1.
  | Card_scheme -> Time_ns.Span.of_day 3.
;;
