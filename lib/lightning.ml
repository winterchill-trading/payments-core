open! Core

let hex_of_bytes bytes = String.concat_map bytes ~f:(fun c -> sprintf "%02x" (Char.to_int c))

let bytes_of_groups groups =
  Bech32.convert_bits groups ~from:5 ~to_:8 ~pad:false
  |> Option.map ~f:(fun l -> String.of_char_list (List.map l ~f:Char.of_int_exn))
;;

(* Big-endian integer from 5-bit groups. *)
let int_of_groups groups = List.fold groups ~init:0 ~f:(fun acc g -> (acc lsl 5) lor g)

module Invoice = struct
  type t =
    { raw : string
    ; network : Network.Stable.V1.t
    ; amount_msat : int option
    ; timestamp : Time_ns.Stable.V1.t
    ; expiry : Time_ns.Span.Stable.V2.t
    ; payment_hash : string
    ; description : string option
    }
  [@@deriving bin_io, sexp, compare, equal, hash]

  (* Longest prefix first, so "lntbs" (signet) is not read as "lntb" + amount "s". *)
  let prefixes =
    [ "lnbcrt", Network.Bitcoin_testnet
    ; "lntbs", Bitcoin_testnet
    ; "lnbc", Bitcoin
    ; "lntb", Bitcoin_testnet
    ]
  ;;

  (* Amount multipliers in millisatoshi per unit: 1 BTC = 10^11 msat. *)
  let amount_msat_of ~digits ~unit =
    let n = Int.of_string digits in
    match unit with
    | None -> Ok (n * 100_000_000_000)
    | Some 'm' -> Ok (n * 100_000_000)
    | Some 'u' -> Ok (n * 100_000)
    | Some 'n' -> Ok (n * 100)
    | Some 'p' ->
      (* BOLT 11: a pico amount must be a whole number of millisatoshi. *)
      if n % 10 = 0 then Ok (n / 10) else Or_error.error_string "sub-millisatoshi amount"
    | Some c -> Or_error.error_s [%message "unknown amount multiplier" (c : char)]
  ;;

  let parse_hrp hrp =
    match List.find prefixes ~f:(fun (p, _) -> String.is_prefix hrp ~prefix:p) with
    | None -> Or_error.error_s [%message "not a Lightning invoice prefix" hrp]
    | Some (p, network) ->
      let amount = String.drop_prefix hrp (String.length p) in
      if String.is_empty amount
      then Ok (network, None)
      else (
        let digits = String.take_while amount ~f:Char.is_digit in
        let rest = String.drop_prefix amount (String.length digits) in
        let unit =
          match String.to_list rest with
          | [] -> Ok None
          | [ c ] -> Ok (Some c)
          | _ -> Or_error.error_s [%message "bad amount" amount]
        in
        if String.is_empty digits || Char.equal digits.[0] '0'
        then Or_error.error_s [%message "bad amount" amount]
        else
          Or_error.bind unit ~f:(fun unit ->
            Or_error.map (amount_msat_of ~digits ~unit) ~f:(fun a -> network, Some a)))
  ;;

  (* Tagged fields: 5-bit type, 10-bit length in groups, then the data. *)
  let rec fields acc = function
    | [] -> Ok (List.rev acc)
    | ty :: l1 :: l2 :: rest ->
      let len = (l1 lsl 5) lor l2 in
      if List.length rest < len
      then Or_error.error_string "truncated tagged field"
      else fields ((ty, List.take rest len) :: acc) (List.drop rest len)
    | _ -> Or_error.error_string "truncated tagged field"
  ;;

  let signature_groups = 104 (* 65 bytes: 64-byte signature + recovery id *)

  let of_string raw =
    let open Or_error.Let_syntax in
    let s = String.lowercase (String.strip raw) in
    let s = Option.value (String.chop_prefix s ~prefix:"lightning:") ~default:s in
    let%bind { hrp; data; variant } =
      match Bech32.decode ~max_length:Int.max_value s with
      | Some ({ variant = Bech32; _ } as d) -> Ok d
      | Some _ | None -> Or_error.error_s [%message "invalid bech32 invoice"]
    in
    ignore (variant : Bech32.Variant.t);
    let%bind network, amount_msat = parse_hrp hrp in
    let n = List.length data in
    let%bind () =
      if n >= 7 + signature_groups then Ok () else Or_error.error_string "invoice too short"
    in
    let timestamp = int_of_groups (List.take data 7) in
    let tagged = List.sub data ~pos:7 ~len:(n - 7 - signature_groups) in
    let%bind fields = fields [] tagged in
    let find ty = List.find_map fields ~f:(fun (t, d) -> Option.some_if (t = ty) d) in
    let%bind payment_hash =
      (* p (1) carries the 32-byte payment hash: 52 groups *)
      match find 1 with
      | Some groups when List.length groups = 52 ->
        (match bytes_of_groups groups with
         | Some b -> Ok (hex_of_bytes b)
         | None -> Or_error.error_string "bad payment hash")
      | _ -> Or_error.error_string "missing payment hash"
    in
    let description = Option.bind (find 13) ~f:bytes_of_groups in
    let expiry =
      Option.value_map (find 6) ~default:(Time_ns.Span.of_hr 1.) ~f:(fun g ->
        Time_ns.Span.of_int_sec (int_of_groups g))
    in
    return
      { raw = s
      ; network
      ; amount_msat
      ; timestamp = Time_ns.of_span_since_epoch (Time_ns.Span.of_int_sec timestamp)
      ; expiry
      ; payment_hash
      ; description
      }
  ;;

  let expires_at t = Time_ns.add t.timestamp t.expiry
  let is_expired t ~now = Time_ns.( >= ) now (expires_at t)
end

module Target = struct
  type t =
    | Invoice of Invoice.t
    | Lnurl of string
    | Address of
        { user : string
        ; domain : string
        }
  [@@deriving bin_io, sexp, compare, equal, hash]

  module Stable = struct
    module V1 = struct
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  let lnurl s =
    match Bech32.decode ~max_length:Int.max_value s with
    | Some { hrp = "lnurl"; data; _ } ->
      (match bytes_of_groups data with
       | Some url
         when String.is_prefix url ~prefix:"https://"
              || (* LUD-01 allows clear http only for onion services *)
              (String.is_prefix url ~prefix:"http://"
               && String.is_suffix
                    (List.hd_exn (String.split (String.drop_prefix url 7) ~on:'/'))
                    ~suffix:".onion") -> Ok (Lnurl url)
       | Some url -> Or_error.error_s [%message "LNURL must be https" url]
       | None -> Or_error.error_string "bad LNURL payload")
    | _ -> Or_error.error_string "invalid LNURL"
  ;;

  let address s =
    match String.split s ~on:'@' with
    | [ user; domain ]
      when (not (String.is_empty user))
           && String.for_all user ~f:(fun c ->
             Char.is_lowercase c || Char.is_digit c || String.mem "-_." c)
           && String.mem domain '.'
           && String.for_all domain ~f:(fun c ->
             Char.is_alphanum c || Char.equal c '-' || Char.equal c '.') ->
      Ok (Address { user; domain = String.lowercase domain })
    | _ -> Or_error.error_s [%message "invalid Lightning address" s]
  ;;

  let of_string raw =
    let s = String.strip raw in
    let lower = String.lowercase s in
    let s = Option.value (String.chop_prefix lower ~prefix:"lightning:") ~default:lower in
    if String.is_prefix s ~prefix:"lnurl1"
    then lnurl s
    else if String.mem s '@'
    then address s
    else Or_error.map (Invoice.of_string s) ~f:(fun i -> Invoice i)
  ;;

  let pay_url = function
    | Invoice _ -> None
    | Lnurl url -> Some url
    | Address { user; domain } -> Some (sprintf "https://%s/.well-known/lnurlp/%s" domain user)
  ;;
end
