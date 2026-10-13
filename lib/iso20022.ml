open! Core

module Sepa_text = struct
  let allowed c = Char.is_alphanum c || String.mem "/-?:().,'+ " c
  let is_valid s = String.for_all s ~f:allowed

  let check ~field ~max_length s =
    if String.is_empty s
    then Or_error.error_s [%message "empty field" field]
    else if String.length s > max_length
    then Or_error.error_s [%message "field too long" field (max_length : int) ~length:(String.length s : int)]
    else if not (is_valid s)
    then Or_error.error_s [%message "characters outside the SEPA set" field s]
    else Ok ()
  ;;

  let check_id ~field s =
    let open Or_error.Let_syntax in
    let%bind () = check ~field ~max_length:35 s in
    if String.is_prefix s ~prefix:"/"
       || String.is_suffix s ~suffix:"/"
       || String.is_substring s ~substring:"//"
    then Or_error.error_s [%message "identifier with a leading, trailing or double slash" field s]
    else Ok ()
  ;;
end

module Uetr = struct
  type t = string [@@deriving bin_io, sexp, compare, equal, hash]

  let of_string raw =
    let s = String.lowercase raw in
    let groups = String.split s ~on:'-' in
    let ok =
      [%equal: int list] (List.map groups ~f:String.length) [ 8; 4; 4; 4; 12 ]
      && List.for_all groups ~f:(String.for_all ~f:Char.is_hex_digit)
      (* version nibble 4, variant nibble 8, 9, a or b *)
      && Char.equal s.[14] '4'
      && String.mem "89ab" s.[19]
    in
    if ok then Ok s else Or_error.error_s [%message "UETR must be a UUID v4" raw]
  ;;

  let to_string t = t

  let create state =
    let hex n = String.init n ~f:(fun _ -> "0123456789abcdef".[Random.State.int state 16]) in
    sprintf
      "%s-%s-4%s-%c%s-%s"
      (hex 8)
      (hex 4)
      (hex 3)
      "89ab".[Random.State.int state 4]
      (hex 3)
      (hex 12)
  ;;
end

module Payment_id = struct
  type t =
    { end_to_end_id : string
    ; instruction_id : string option
    ; uetr : Uetr.t option
    }
  [@@deriving bin_io, sexp, compare, equal, hash]

  let not_provided = "NOTPROVIDED"

  let create ~end_to_end_id ?instruction_id ?uetr () =
    let open Or_error.Let_syntax in
    let%bind () = Sepa_text.check_id ~field:"end_to_end_id" end_to_end_id in
    let%map () =
      match instruction_id with
      | None -> Ok ()
      | Some id -> Sepa_text.check_id ~field:"instruction_id" id
    in
    { end_to_end_id; instruction_id; uetr }
  ;;
end

module Creditor_reference = struct
  type t = string [@@deriving bin_io, sexp, compare, equal, hash]

  (* Same mod-97 arithmetic as IBAN check digits. *)
  let mod97 s =
    String.fold s ~init:0 ~f:(fun acc c ->
      if Char.is_digit c
      then ((acc * 10) + Char.get_digit_exn c) % 97
      else ((acc * 100) + (Char.to_int (Char.uppercase c) - Char.to_int 'A' + 10)) % 97)
  ;;

  let normalize s = String.filter s ~f:(fun c -> not (Char.is_whitespace c)) |> String.uppercase

  let of_string raw =
    let s = normalize raw in
    let body = String.drop_prefix s 4 in
    if String.is_prefix s ~prefix:"RF"
       && String.length s >= 5
       && String.length s <= 25
       && String.for_all (String.sub s ~pos:2 ~len:2) ~f:Char.is_digit
       && String.for_all body ~f:Char.is_alphanum
       && mod97 (body ^ String.prefix s 4) = 1
    then Ok s
    else Or_error.error_s [%message "invalid ISO 11649 creditor reference" raw]
  ;;

  let to_string t = t

  let of_body raw =
    let body = normalize raw in
    if String.is_empty body || String.length body > 21 || not (String.for_all body ~f:Char.is_alphanum)
    then Or_error.error_s [%message "reference body must be 1-21 alphanumerics" raw]
    else of_string (sprintf "RF%02d%s" (98 - mod97 (body ^ "RF00")) body)
  ;;
end

module Remittance = struct
  type t =
    | Unstructured of string
    | Structured of Creditor_reference.t
    | None_given
  [@@deriving bin_io, sexp, compare, equal, hash]

  let unstructured s =
    Or_error.map (Sepa_text.check ~field:"remittance" ~max_length:140 s) ~f:(fun () ->
      Unstructured s)
  ;;
end

module Postal_address = struct
  type t =
    { country : string
    ; town : string
    ; post_code : string option
    ; street : string option
    ; building_number : string option
    ; lines : string list
    }
  [@@deriving bin_io, sexp, compare, equal, hash]

  let validate t =
    let open Or_error.Let_syntax in
    let%bind () =
      if String.length t.country = 2 && String.for_all t.country ~f:Char.is_uppercase
      then Ok ()
      else Or_error.error_s [%message "country must be ISO 3166 alpha-2" t.country]
    in
    let%bind () = Sepa_text.check ~field:"town" ~max_length:35 t.town in
    let opt field max_length = function
      | None -> Ok ()
      | Some s -> Sepa_text.check ~field ~max_length s
    in
    let%bind () = opt "post_code" 16 t.post_code in
    let%bind () = opt "street" 70 t.street in
    let%bind () = opt "building_number" 16 t.building_number in
    if List.length t.lines > 2
    then Or_error.error_string "at most two address lines"
    else
      List.map t.lines ~f:(Sepa_text.check ~field:"address line" ~max_length:70)
      |> Or_error.all_unit
  ;;
end

module Party = struct
  type t =
    { name : string
    ; iban : Iban.Stable.V1.t
    ; bic : Bic.Stable.V1.t option
    ; address : Postal_address.t option
    }
  [@@deriving bin_io, sexp, compare, equal, hash]

  let validate t =
    let open Or_error.Let_syntax in
    let%bind () = Sepa_text.check ~field:"name" ~max_length:70 t.name in
    match t.address with
    | None -> Ok ()
    | Some a -> Postal_address.validate a
  ;;
end

module Charge_bearer = struct
  type t =
    | SLEV
    | DEBT
    | CRED
    | SHAR
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]
end

module Transaction_status = struct
  type t =
    | RCVD
    | ACTC
    | ACCP
    | ACSP
    | ACWC
    | PDNG
    | ACSC
    | ACCC
    | RJCT
    | CANC
  [@@deriving bin_io, sexp, compare, equal, hash, enumerate]

  let of_string s =
    match List.find all ~f:(fun t -> String.equal (Sexp.to_string (sexp_of_t t)) (String.uppercase s)) with
    | Some t -> Ok t
    | None -> Or_error.error_s [%message "unknown transaction status" s]
  ;;
end
