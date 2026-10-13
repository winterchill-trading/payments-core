open! Core
module Amount = Money.Amount
module Account_id = Ledger.Account_id

module Basis = struct
  type t =
    | Posted
    | Available
  [@@deriving sexp, compare, equal]
end

module Reported_balance = struct
  type t =
    { account : Account_id.t
    ; amount : Amount.t
    ; basis : Basis.t
    }
  [@@deriving sexp, compare, equal]
end

module Reported_transfer = struct
  type t =
    { provider_ref : string
    ; status : Transfer.Status.t
    }
  [@@deriving sexp, compare, equal]
end

module Diff = struct
  type t =
    | Balance_mismatch of
        { account : Account_id.t
        ; basis : Basis.t
        ; ledger : Amount.t
        ; provider : Amount.t
        ; delta : Amount.t
        }
    | Unknown_account of { account : Account_id.t }
    | Unreported_balance of
        { account : Account_id.t
        ; ledger : Amount.t
        }
    | Status_mismatch of
        { transfer : Transfer.Id.t
        ; provider_ref : string
        ; ledger : Transfer.Status.t
        ; provider : Transfer.Status.t
        }
    | Unknown_transfer of
        { provider_ref : string
        ; provider : Transfer.Status.t
        }
    | Missing_at_provider of
        { transfer : Transfer.Id.t
        ; provider_ref : string
        }
  [@@deriving sexp, compare, equal]
end

let ledger_figure balances account currency (basis : Basis.t) =
  match basis with
  | Posted -> Ledger.Balances.posted balances account currency
  | Available -> Ledger.Balances.available balances account currency
;;

let balances ?(scope = []) balances reported =
  let compared =
    List.filter_map reported ~f:(fun (r : Reported_balance.t) ->
      match Ledger.Balances.find_account balances r.account with
      | None -> Some (Diff.Unknown_account { account = r.account })
      | Some _ ->
        let ledger = ledger_figure balances r.account (Amount.currency r.amount) r.basis in
        if Amount.equal ledger r.amount
        then None
        else
          Some
            (Balance_mismatch
               { account = r.account
               ; basis = r.basis
               ; ledger
               ; provider = r.amount
               ; delta = Amount.sub_exn r.amount ledger
               }))
  in
  let reported_keys =
    List.map reported ~f:(fun r -> r.account, Amount.currency r.amount)
  in
  let unreported =
    List.filter_map scope ~f:(fun (account, currency) ->
      if List.mem
           reported_keys
           (account, currency)
           ~equal:[%equal: Account_id.t * Money.Currency.t]
      then None
      else (
        let ledger = Ledger.Balances.posted balances account currency in
        if Amount.is_zero ledger
        then None
        else Some (Diff.Unreported_balance { account; ledger })))
  in
  compared @ unreported
;;

let transfers (ours : Transfer.t list) (theirs : Reported_transfer.t list) =
  let by_ref =
    List.filter_map ours ~f:(fun t -> Option.map t.provider_ref ~f:(fun r -> r, t))
    |> String.Map.of_alist_reduce ~f:(fun a _ -> a)
  in
  let reported = String.Map.of_alist_reduce (List.map theirs ~f:(fun r -> r.provider_ref, r.status)) ~f:(fun a _ -> a) in
  let from_provider =
    Map.to_alist reported
    |> List.filter_map ~f:(fun (provider_ref, provider) ->
      match Map.find by_ref provider_ref with
      | None -> Some (Diff.Unknown_transfer { provider_ref; provider })
      | Some t ->
        if Transfer.Status.equal t.status provider
        then None
        else Some (Status_mismatch { transfer = t.id; provider_ref; ledger = t.status; provider }))
  in
  let missing =
    Map.to_alist by_ref
    |> List.filter_map ~f:(fun (provider_ref, (t : Transfer.t)) ->
      match t.status with
      | (Submitted | Pending) when not (Map.mem reported provider_ref) ->
        Some (Diff.Missing_at_provider { transfer = t.id; provider_ref })
      | _ -> None)
  in
  from_provider @ missing
;;
