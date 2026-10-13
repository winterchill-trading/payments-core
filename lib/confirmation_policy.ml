open! Core

type t =
  | Confirmations of int
  | Finalized
[@@deriving sexp, compare, equal]

let large_eur = Money.Amount.of_decimal_string Money.Currency.eur "10000" |> Or_error.ok_exn

let required (network : Network.t) ~eur_value =
  match network with
  | Bitcoin | Bitcoin_testnet ->
    let large =
      match eur_value with
      | None -> true
      | Some v ->
        (match Money.Amount.compare_value v large_eur with
         | Ok c -> c > 0
         | Error _ -> true)
    in
    Confirmations (if large then 6 else 3)
  | Ethereum | Polygon -> Finalized
  (* Not covered by the research note: Tron's solidified block is 19 confirmations;
     Solana's "finalized" commitment is about 32 slots. *)
  | Tron -> Confirmations 19
  | Solana -> Finalized
;;

let is_met t ~confirmations ~finalized =
  match t with
  | Confirmations n -> confirmations >= n
  | Finalized -> finalized
;;
