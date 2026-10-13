open! Core

let alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

let index =
  let a = Array.create ~len:256 (-1) in
  String.iteri alphabet ~f:(fun i c -> a.(Char.to_int c) <- i);
  a
;;

(* Schoolbook base conversion over a byte array; addresses are under 64 bytes so the
   quadratic cost does not matter. *)
let encode bytes =
  let zeros = String.take_while bytes ~f:(Char.equal '\000') |> String.length in
  let digits = Queue.create () in
  (* digits holds base-58 digits, least significant first *)
  String.iter bytes ~f:(fun c ->
    let carry = ref (Char.to_int c) in
    for i = 0 to Queue.length digits - 1 do
      let x = (Queue.get digits i * 256) + !carry in
      Queue.set digits i (x % 58);
      carry := x / 58
    done;
    while !carry > 0 do
      Queue.enqueue digits (!carry % 58);
      carry := !carry / 58
    done);
  let body = Queue.to_list digits |> List.rev_map ~f:(fun d -> alphabet.[d]) in
  String.make zeros '1' ^ String.of_char_list body
;;

let decode s =
  with_return (fun { return } ->
    let zeros = String.take_while s ~f:(Char.equal '1') |> String.length in
    let bytes = Queue.create () in
    String.iter s ~f:(fun c ->
      let d = index.(Char.to_int c) in
      if d < 0 then return None;
      let carry = ref d in
      for i = 0 to Queue.length bytes - 1 do
        let x = (Queue.get bytes i * 58) + !carry in
        Queue.set bytes i (x land 0xff);
        carry := x lsr 8
      done;
      while !carry > 0 do
        Queue.enqueue bytes (!carry land 0xff);
        carry := !carry lsr 8
      done);
    let body = Queue.to_list bytes |> List.rev_map ~f:Char.of_int_exn in
    Some (String.make zeros '\000' ^ String.of_char_list body))
;;

let checksum payload =
  let sha = Cryptokit.Hash.sha256 in
  let once = Cryptokit.hash_string (sha ()) payload in
  String.prefix (Cryptokit.hash_string (sha ()) once) 4
;;

let encode_check payload = encode (payload ^ checksum payload)

let decode_check s =
  match decode s with
  | Some raw when String.length raw >= 4 ->
    let payload = String.drop_suffix raw 4 in
    if String.equal (String.suffix raw 4) (checksum payload) then Some payload else None
  | _ -> None
;;
