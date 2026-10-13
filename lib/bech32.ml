open! Core

module Variant = struct
  type t =
    | Bech32
    | Bech32m
  [@@deriving sexp, compare, equal]

  let constant = function
    | Bech32 -> 1
    | Bech32m -> 0x2bc830a3
  ;;
end

type decoded =
  { hrp : string
  ; data : int list
  ; variant : Variant.t
  }
[@@deriving sexp, compare, equal]

let charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

let charset_rev =
  let a = Array.create ~len:128 (-1) in
  String.iteri charset ~f:(fun i c -> a.(Char.to_int c) <- i);
  a
;;

let generator = [| 0x3b6a57b2; 0x26508e6d; 0x1ea119fa; 0x3d4233dd; 0x2a1462b3 |]

let polymod values =
  List.fold values ~init:1 ~f:(fun chk v ->
    let b = chk lsr 25 in
    let chk = ref (((chk land 0x1ffffff) lsl 5) lxor v) in
    for i = 0 to 4 do
      if (b lsr i) land 1 = 1 then chk := !chk lxor generator.(i)
    done;
    !chk)
;;

let hrp_expand hrp =
  let cs = String.to_list hrp |> List.map ~f:Char.to_int in
  List.map cs ~f:(fun c -> c lsr 5) @ [ 0 ] @ List.map cs ~f:(fun c -> c land 31)
;;

let checksum ~hrp ~data variant =
  let pm =
    polymod (hrp_expand hrp @ data @ [ 0; 0; 0; 0; 0; 0 ]) lxor Variant.constant variant
  in
  List.init 6 ~f:(fun i -> (pm lsr (5 * (5 - i))) land 31)
;;

let encode ~hrp ~data variant =
  let hrp = String.lowercase hrp in
  hrp
  ^ "1"
  ^ String.of_char_list
      (List.map (data @ checksum ~hrp ~data variant) ~f:(fun d -> charset.[d]))
;;

let decode ?(max_length = 90) s =
  let len = String.length s in
  let lower = String.lowercase s in
  let mixed = (not (String.equal s lower)) && not (String.equal s (String.uppercase s)) in
  let printable = String.for_all s ~f:(fun c -> Char.to_int c >= 33 && Char.to_int c <= 126) in
  match String.rindex lower '1' with
  | None -> None
  | Some pos when mixed || (not printable) || pos < 1 || pos + 7 > len || len > max_length
    -> None
  | Some pos ->
    let hrp = String.prefix lower pos in
    let rest = String.drop_prefix lower (pos + 1) |> String.to_list in
    let values = List.map rest ~f:(fun c -> charset_rev.(Char.to_int c land 127)) in
    if List.exists values ~f:(fun v -> v < 0)
    then None
    else (
      let pm = polymod (hrp_expand hrp @ values) in
      let variant =
        if pm = Variant.constant Bech32
        then Some Variant.Bech32
        else if pm = Variant.constant Bech32m
        then Some Variant.Bech32m
        else None
      in
      Option.map variant ~f:(fun variant ->
        { hrp; data = List.take values (List.length values - 6); variant }))
;;

let convert_bits values ~from ~to_ ~pad =
  let maxv = (1 lsl to_) - 1 in
  with_return (fun { return } ->
    let acc, bits, out =
      List.fold values ~init:(0, 0, []) ~f:(fun (acc, bits, out) v ->
        if v < 0 || v lsr from <> 0 then return None;
        let acc = (acc lsl from) lor v in
        let bits = ref (bits + from) in
        let out = ref out in
        while !bits >= to_ do
          bits := !bits - to_;
          out := ((acc lsr !bits) land maxv) :: !out
        done;
        (* keep only the bits not yet emitted so acc never overflows *)
        acc land ((1 lsl !bits) - 1), !bits, !out)
    in
    if pad
    then Some (List.rev (if bits > 0 then ((acc lsl (to_ - bits)) land maxv) :: out else out))
    else if bits >= from || (acc lsl (to_ - bits)) land maxv <> 0
    then None
    else Some (List.rev out))
;;

let bytes_of s = String.to_list s |> List.map ~f:Char.to_int

let decode_segwit ~hrp s =
  let open Option.Let_syntax in
  let%bind { hrp = got; data; variant } = decode s in
  let%bind version, rest =
    match data with
    | v :: rest when String.equal got hrp -> Some (v, rest)
    | _ -> None
  in
  let%bind program = convert_bits rest ~from:5 ~to_:8 ~pad:false in
  let n = List.length program in
  let ok_variant =
    match version, variant with
    | 0, Bech32 -> n = 20 || n = 32
    | v, Bech32m -> v >= 1 && v <= 16 && n >= 2 && n <= 40
    | _ -> false
  in
  if ok_variant
  then Some (version, String.of_char_list (List.map program ~f:Char.of_int_exn))
  else None
;;

let encode_segwit ~hrp ~version ~program =
  let data =
    version :: Option.value_exn (convert_bits (bytes_of program) ~from:8 ~to_:5 ~pad:true)
  in
  encode ~hrp ~data (if version = 0 then Bech32 else Bech32m)
;;
