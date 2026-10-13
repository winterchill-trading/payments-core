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

module Internal_tx = struct
  type t =
    { provider_ref : string
    ; amount : Amount.t
    ; booked_on : Date.t
    }
  [@@deriving sexp, compare, equal]
end

module External_tx = struct
  type t =
    { provider_ref : string
    ; amount : Amount.t
    ; value_date : Date.t
    }
  [@@deriving sexp, compare, equal]
end

let business_days_between a b =
  (* business days strictly after [a] up to and including [b] *)
  let rec go d n =
    if Date.( >= ) d b
    then n
    else (
      let d = Date.add_days d 1 in
      go d (if Target_calendar.is_business_day d then n + 1 else n))
  in
  go a 0
;;

module Break = struct
  module Kind = struct
    type t =
      | Timing
      | Missing_internal
      | Missing_external
      | Amount_mismatch
      | Duplicate
    [@@deriving sexp, compare, equal, enumerate]
  end

  module Status = struct
    type t =
      | Open
      | Explained of string
      | Resolved of
          { adjustment : Ledger.Idempotency_key.t
          ; maker : string
          ; checker : string
          }
    [@@deriving sexp, compare, equal]
  end

  type t =
    { id : string
    ; kind : Kind.t
    ; provider_ref : string
    ; internal : Amount.t option
    ; external_ : Amount.t option
    ; first_seen : Date.t
    ; status : Status.t
    }
  [@@deriving sexp, compare, equal]

  module Age = struct
    type t =
      | Days_0_1
      | Days_2_5
      | Over_5
    [@@deriving sexp, compare, equal, enumerate]
  end

  let age t ~today =
    match business_days_between t.first_seen today with
    | n when n <= 1 -> Age.Days_0_1
    | n when n <= 5 -> Days_2_5
    | _ -> Over_5
  ;;

  let explain t why = { t with status = Explained why }

  let resolve t ~(adjustment : Ledger.Entry.t) ~maker ~checker =
    if String.equal maker checker
    then Or_error.error_s [%message "maker and checker must differ" maker]
    else if not (String.is_substring adjustment.reference ~substring:t.id)
    then
      Or_error.error_s
        [%message "adjustment does not reference the break" ~break:t.id adjustment.reference]
    else
      Ok
        { t with
          status = Resolved { adjustment = adjustment.idempotency_key; maker; checker }
        }
  ;;

  let create ~kind ~provider_ref ?internal ?external_ ~first_seen () =
    let id = sprintf "%s:%s" (Sexp.to_string [%sexp (kind : Kind.t)]) provider_ref in
    { id; kind; provider_ref; internal; external_; first_seen; status = Open }
  ;;
end

let match_transactions ?(timing_days = 1) ~business_date ours theirs =
  let recent date = business_days_between date business_date <= timing_days in
  let ours_by = String.Map.of_alist_multi (List.map ours ~f:(fun (t : Internal_tx.t) -> t.provider_ref, t)) in
  let theirs_by =
    String.Map.of_alist_reduce
      (List.map theirs ~f:(fun (t : External_tx.t) -> t.provider_ref, t))
      ~f:(fun a _ -> a)
  in
  let first_seen = business_date in
  let from_ours =
    Map.to_alist ours_by
    |> List.concat_map ~f:(fun (provider_ref, (txs : Internal_tx.t list)) ->
      let first = List.hd_exn txs in
      let duplicate =
        if List.length txs > 1
        then
          [ Break.create ~kind:Duplicate ~provider_ref ~internal:first.amount ~first_seen () ]
        else []
      in
      let main =
        match Map.find theirs_by provider_ref with
        | Some ext when Amount.equal ext.amount first.amount -> []
        | Some ext ->
          [ Break.create
              ~kind:Amount_mismatch
              ~provider_ref
              ~internal:first.amount
              ~external_:ext.amount
              ~first_seen
              ()
          ]
        | None ->
          let kind : Break.Kind.t = if recent first.booked_on then Timing else Missing_external in
          [ Break.create ~kind ~provider_ref ~internal:first.amount ~first_seen () ]
      in
      duplicate @ main)
  in
  let from_theirs =
    Map.to_alist theirs_by
    |> List.filter_map ~f:(fun (provider_ref, (ext : External_tx.t)) ->
      if Map.mem ours_by provider_ref
      then None
      else (
        let kind : Break.Kind.t = if recent ext.value_date then Timing else Missing_internal in
        Some (Break.create ~kind ~provider_ref ~external_:ext.amount ~first_seen ())))
  in
  from_ours @ from_theirs
;;

let carry_forward ~previous ~current =
  let prev = String.Map.of_alist_reduce (List.map previous ~f:(fun (b : Break.t) -> b.id, b)) ~f:(fun a _ -> a) in
  let cur_ids = String.Set.of_list (List.map current ~f:(fun (b : Break.t) -> b.id)) in
  let merged =
    List.map current ~f:(fun (b : Break.t) ->
      match Map.find prev b.id with
      | Some p -> { b with first_seen = p.first_seen; status = p.status }
      | None ->
        (* A timing break that turned into a missing one keeps the original date. *)
        let timing_id = sprintf "Timing:%s" b.provider_ref in
        (match Map.find prev timing_id with
         | Some p -> { b with first_seen = p.first_seen }
         | None -> b))
  in
  let still_open =
    List.filter previous ~f:(fun (b : Break.t) ->
      (not (Set.mem cur_ids b.id))
      &&
      match b.kind, b.status with
      | Timing, _ -> false
      | _, Resolved _ -> false
      | _, (Open | Explained _) -> true)
  in
  merged @ still_open
;;

let ageing breaks ~today =
  let open_ =
    List.filter breaks ~f:(fun (b : Break.t) ->
      match b.status with
      | Resolved _ -> false
      | Open | Explained _ -> true)
  in
  List.map Break.Age.all ~f:(fun bucket ->
    ( bucket
    , List.count open_ ~f:(fun b -> Break.Age.equal (Break.age b ~today) bucket) ))
;;
