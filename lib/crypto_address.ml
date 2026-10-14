open! Core

module T = struct
  type t =
    { network : Network.Stable.V1.t
    ; address : string
    }
  [@@deriving bin_io, sexp, compare, equal, hash, fields]
end

include T

let is_hex s = String.for_all s ~f:Char.is_hex_digit

let eip55 s =
  let body = Option.value (String.chop_prefix s ~prefix:"0x") ~default:s in
  if String.length body <> 40 || not (is_hex body)
  then None
  else (
    let lower = String.lowercase body in
    let hash = Cryptokit.hash_string (Cryptokit.Hash.keccak 256) lower in
    (* Nibble i of keccak256(lowercase hex) >= 8 means upper-case digit i. *)
    let nibble i =
      let byte = Char.to_int hash.[i / 2] in
      if i % 2 = 0 then byte lsr 4 else byte land 0xf
    in
    Some
      ("0x"
       ^ String.mapi lower ~f:(fun i c -> if nibble i >= 8 then Char.uppercase c else c)))
;;

let err network raw why =
  Or_error.error_s [%message "invalid address" (network : Network.t) raw why]
;;

let bitcoin ~network ~versions ~hrps raw =
  let base58 () =
    match Base58.decode_check raw with
    | Some p when String.length p = 21 && List.mem versions p.[0] ~equal:Char.equal ->
      Ok { network; address = raw }
    | Some _ -> err network raw "wrong version byte or length"
    | None -> err network raw "bad base58check"
  in
  let lower = String.lowercase raw in
  match List.find hrps ~f:(fun hrp -> String.is_prefix lower ~prefix:(hrp ^ "1")) with
  | Some hrp ->
    (match Bech32.decode_segwit ~hrp raw with
     | Some _ -> Ok { network; address = lower }
     | None -> err network raw "bad segwit address")
  | None -> base58 ()
;;

let create (network : Network.t) raw =
  let raw = String.strip raw in
  match network with
  | Bitcoin -> bitcoin ~network ~versions:[ '\x00'; '\x05' ] ~hrps:[ "bc" ] raw
  | Bitcoin_testnet ->
    (* tb covers testnet and signet; bcrt is regtest, handy in local test setups. *)
    bitcoin ~network ~versions:[ '\x6f'; '\xc4' ] ~hrps:[ "tb"; "bcrt" ] raw
  | Ethereum | Polygon | Base | Arbitrum | Plasma ->
    (match String.chop_prefix raw ~prefix:"0x", eip55 raw with
     | Some body, Some canonical ->
       let single_case =
         String.equal body (String.lowercase body)
         || String.equal body (String.uppercase body)
       in
       if single_case || String.equal raw canonical
       then Ok { network; address = canonical }
       else err network raw "EIP-55 checksum mismatch"
     | _ -> err network raw "expected 0x and 40 hex digits")
  | Tron ->
    (match Base58.decode_check raw with
     | Some p when String.length p = 21 && Char.equal p.[0] '\x41' ->
       Ok { network; address = raw }
     | _ -> err network raw "expected base58check of 0x41 and 20 bytes")
  | Solana ->
    (match Base58.decode raw with
     | Some p when String.length p = 32 -> Ok { network; address = raw }
     | _ -> err network raw "expected base58 of 32 bytes")
  | Spark ->
    (* Spark addresses are Bech32m with hrp [sp] (mainnet), [spt] (testnet), [sprt]
       (regtest) or [sps] (signet). Only the checksum and the prefix are checked: the
       payload layout is the operator's and may change. *)
    let lower = String.lowercase raw in
    (match Bech32.decode ~max_length:Int.max_value raw with
     | Some { hrp; data = _ :: _; variant = Bech32m }
       when List.mem [ "sp"; "spt"; "sprt"; "sps" ] hrp ~equal:String.equal ->
       Ok { network; address = lower }
     | _ ->
       err network raw "expected a Bech32m address with an sp, spt, sprt or sps prefix")
;;

let create_exn network raw = create network raw |> Or_error.ok_exn

module Stable = struct
  module V1 = T

  module V2 = struct
    type t = T.t =
      { network : Network.Stable.V2.t
      ; address : string
      }
    [@@deriving bin_io, sexp, compare, equal, hash]
  end
end
