open! Core

module Stable = struct
  module V2 = struct
    type t =
      | Sepa_credit
      | Sepa_instant
      | Onchain of Network.Stable.V1.t
      | Lightning
      | Intraledger
      | Card_scheme
      | Faster_payments
      | Chaps
      | Bacs
    [@@deriving bin_io, sexp, compare, equal, hash, quickcheck]
  end

  module V1 = struct
    (* The original six constructors, in their original order: the bin_prot shape (and so
       the pinned digest) comes from this type. *)
    module Repr = struct
      type t =
        | Sepa_credit
        | Sepa_instant
        | Onchain of Network.Stable.V1.t
        | Lightning
        | Intraledger
        | Card_scheme
      [@@deriving bin_io, quickcheck]
    end

    type t = V2.t [@@deriving sexp, compare, equal, hash]

    let of_repr : Repr.t -> t = function
      | Sepa_credit -> Sepa_credit
      | Sepa_instant -> Sepa_instant
      | Onchain n -> Onchain n
      | Lightning -> Lightning
      | Intraledger -> Intraledger
      | Card_scheme -> Card_scheme
    ;;

    let to_repr : t -> Repr.t = function
      | Sepa_credit -> Sepa_credit
      | Sepa_instant -> Sepa_instant
      | Onchain n -> Onchain n
      | Lightning -> Lightning
      | Intraledger -> Intraledger
      | Card_scheme -> Card_scheme
      | (Faster_payments | Chaps | Bacs) as rail ->
        raise_s
          [%message
            "Rail.Stable.V1 cannot hold this rail; write Rail.Stable.V2" (rail : V2.t)]
    ;;

    include
      Binable.Of_binable_without_uuid [@alert "-legacy"]
        (Repr)
        (struct
          type nonrec t = t

          let to_binable = to_repr
          let of_binable = of_repr
        end)

    let quickcheck_generator =
      Quickcheck.Generator.map Repr.quickcheck_generator ~f:of_repr
    ;;

    let quickcheck_observer = V2.quickcheck_observer
    let quickcheck_shrinker : t Quickcheck.Shrinker.t = Quickcheck.Shrinker.empty ()
  end
end

type t = Stable.V2.t =
  | Sepa_credit
  | Sepa_instant
  | Onchain of Network.t
  | Lightning
  | Intraledger
  | Card_scheme
  | Faster_payments
  | Chaps
  | Bacs
[@@deriving sexp, compare, equal, hash, quickcheck]

include (Stable.V1 : Binable.S with type t := t)

let all =
  [ Sepa_credit; Sepa_instant ]
  @ List.map Network.all ~f:(fun n -> Onchain n)
  @ [ Lightning; Intraledger; Card_scheme; Faster_payments; Chaps; Bacs ]
;;

let to_string = function
  | Sepa_credit -> "sepa_credit"
  | Sepa_instant -> "sepa_instant"
  | Onchain n -> "onchain:" ^ Network.to_string n
  | Lightning -> "lightning"
  | Intraledger -> "intraledger"
  | Card_scheme -> "card_scheme"
  | Faster_payments -> "faster_payments"
  | Chaps -> "chaps"
  | Bacs -> "bacs"
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
  | Sepa_instant | Lightning | Intraledger | Card_scheme | Faster_payments -> true
  | Sepa_credit | Onchain _ | Chaps | Bacs -> false
;;

let is_returnable = function
  | Sepa_credit | Sepa_instant | Card_scheme | Faster_payments | Chaps | Bacs -> true
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
  (* Most Faster Payments arrive in seconds; the scheme lets a receiving bank hold one for
     up to two hours (for example for a fraud check). *)
  | Faster_payments -> Time_ns.Span.of_hr 2.
  (* CHAPS settles the same business day; a payment after the cut-off goes the next one,
     and a weekend sits in between. *)
  | Chaps -> Time_ns.Span.of_day 3.
  (* Bacs is a three-working-day cycle (submit, process, settle); a weekend adds two. *)
  | Bacs -> Time_ns.Span.of_day 5.
;;

let is_uk = function
  | Faster_payments | Chaps | Bacs -> true
  | Sepa_credit | Sepa_instant | Onchain _ | Lightning | Intraledger | Card_scheme ->
    false
;;
