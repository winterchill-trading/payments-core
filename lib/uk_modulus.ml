open! Core

module Method = struct
  type t =
    | Mod10
    | Mod11
    | Dblal
  [@@deriving sexp, compare, equal, enumerate]

  let of_string = function
    | "MOD10" -> Ok Mod10
    | "MOD11" -> Ok Mod11
    | "DBLAL" -> Ok Dblal
    | s -> Or_error.error_s [%message "unknown modulus method" s]
  ;;
end

module Rule = struct
  type t =
    { first : Sort_code.t
    ; last : Sort_code.t
    ; method_ : Method.t
    ; weights : int list
    ; exception_ : int option
    }
  [@@deriving sexp, compare, equal]

  let of_string line =
    let open Or_error.Let_syntax in
    match String.split line ~on:' ' |> List.filter ~f:(Fn.non String.is_empty) with
    | first :: last :: method_ :: rest ->
      let%bind first = Sort_code.of_string first in
      let%bind last = Sort_code.of_string last in
      let%bind method_ = Method.of_string method_ in
      let ints l = Or_error.try_with (fun () -> List.map l ~f:Int.of_string) in
      let%bind weights, exception_ =
        match List.length rest with
        | 14 -> ints rest >>| fun w -> w, None
        | 15 ->
          let%map w = ints (List.take rest 14)
          and e = ints (List.drop rest 14) in
          w, List.hd e
        | n -> Or_error.error_s [%message "expected 14 weights" ~got:(n : int) line]
      in
      return { first; last; method_; weights; exception_ }
    | _ -> Or_error.error_s [%message "not a weight table line" line]
  ;;
end

module Table = struct
  type t =
    { rules : Rule.t list
    ; substitutions : Sort_code.t String.Map.t
    }

  let create ?(substitutions = []) rules =
    (* Later lines win if the substitution file repeats a sort code. *)
    { rules
    ; substitutions =
        List.map substitutions ~f:(fun (k, v) -> Sort_code.to_string k, v)
        |> String.Map.of_alist_reduce ~f:(fun _ b -> b)
    }
  ;;

  let empty = create []

  let lines s =
    String.split_lines s
    |> List.map ~f:String.strip
    |> List.filter ~f:(Fn.non String.is_empty)
  ;;

  let of_string ?(substitutions = "") weights =
    let open Or_error.Let_syntax in
    let%bind rules =
      lines weights |> List.map ~f:Rule.of_string |> Or_error.combine_errors
    in
    let%map substitutions =
      lines substitutions
      |> List.map ~f:(fun line ->
        match String.split line ~on:' ' |> List.filter ~f:(Fn.non String.is_empty) with
        | [ a; b ] ->
          let%map a = Sort_code.of_string a
          and b = Sort_code.of_string b in
          a, b
        | _ -> Or_error.error_s [%message "not a substitution line" line])
      |> Or_error.combine_errors
    in
    create ~substitutions rules
  ;;

  let rules t sort_code =
    let n = Sort_code.to_int sort_code in
    List.filter t.rules ~f:(fun (r : Rule.t) ->
      Sort_code.to_int r.first <= n && n <= Sort_code.to_int r.last)
  ;;
end

module Result = struct
  type t =
    | Valid
    | Invalid
    | Not_checked
  [@@deriving sexp, compare, equal, enumerate]
end

let normalize_account_number raw =
  let s =
    String.filter raw ~f:(fun c -> not (Char.equal c '-' || Char.is_whitespace c))
  in
  if not (String.for_all s ~f:Char.is_digit)
  then Or_error.error_s [%message "account number must be digits" (raw : string)]
  else (
    match String.length s with
    | 6 -> Ok ("00" ^ s)
    | 7 -> Ok ("0" ^ s)
    | 8 -> Ok s
    | n ->
      Or_error.error_s
        [%message
          "account number must have six to eight digits; nine and ten digit numbers need \
           the bank's own conversion"
            ~digits:(n : int)])
;;

(* Positions in the 14 digits [u v w x y z a b c d e f g h]. *)
let a = 6
let b = 7
let c = 8
let g = 12
let h = 13

let digits ~sort_code ~account =
  Array.of_list (List.map (String.to_list (sort_code ^ account)) ~f:Char.get_digit_exn)
;;

(* One check from one table line, with the exceptions that change the weights or the
   total. [d] already has any substituted sort code. *)
let run_check (rule : Rule.t) (d : int array) =
  let weights = Array.of_list rule.weights in
  let zero_u_to_b () =
    for i = 0 to b do
      weights.(i) <- 0
    done
  in
  (match rule.exception_ with
   | Some 7 when d.(g) = 9 -> zero_u_to_b ()
   | Some 10 when (d.(a) = 0 || d.(a) = 9) && d.(b) = 9 && d.(g) = 9 -> zero_u_to_b ()
   | Some 2 when d.(a) <> 0 ->
     let w =
       if d.(g) <> 9
       then [| 0; 0; 1; 2; 5; 3; 6; 4; 8; 7; 10; 9; 3; 1 |]
       else [| 0; 0; 0; 0; 0; 0; 0; 0; 8; 7; 10; 9; 3; 1 |]
     in
     Array.blit ~src:w ~src_pos:0 ~dst:weights ~dst_pos:0 ~len:14
   | _ -> ());
  let products = Array.map2_exn d weights ~f:( * ) in
  let total =
    match rule.method_ with
    | Dblal ->
      (* Double alternate sums the digits of each product (14 counts as 1 + 4). *)
      Array.sum (module Int) products ~f:(fun p ->
        String.sum (module Int) (Int.to_string (abs p)) ~f:Char.get_digit_exn)
    | Mod10 | Mod11 -> Array.sum (module Int) products ~f:Fn.id
  in
  let total =
    if Option.equal Int.equal rule.exception_ (Some 1) then total + 27 else total
  in
  match rule.method_, rule.exception_ with
  | Mod11, Some 4 -> total % 11 = (10 * d.(g)) + d.(h)
  | Mod11, Some 5 ->
    (match total % 11 with
     | 0 -> d.(g) = 0
     | 1 -> false
     | r -> 11 - r = d.(g))
  | Dblal, Some 5 ->
    (match total % 10 with
     | 0 -> d.(h) = 0
     | r -> 10 - r = d.(h))
  | Mod11, _ -> total % 11 = 0
  | (Mod10 | Dblal), _ -> total % 10 = 0
;;

let check (table : Table.t) ~sort_code ~account_number =
  let open Or_error.Let_syntax in
  let%map account = normalize_account_number account_number in
  let sc = Sort_code.to_string sort_code in
  let with_sort_code (rule : Rule.t) =
    let sc =
      match rule.exception_ with
      | Some 5 ->
        Map.find table.substitutions sc
        |> Option.value_map ~default:sc ~f:Sort_code.to_string
      | Some 8 -> "090126"
      | _ -> sc
    in
    digits ~sort_code:sc ~account
  in
  let check1 rule = run_check rule (with_sort_code rule) in
  let of_bool ok : Result.t = if ok then Valid else Invalid in
  let d = digits ~sort_code:sc ~account in
  match Table.rules table sort_code with
  | [] -> Result.Not_checked
  | first :: _
    when Option.equal Int.equal first.exception_ (Some 6)
         && d.(a) >= 4
         && d.(a) <= 8
         && d.(g) = d.(h) ->
    (* A foreign currency account at these banks: the number cannot be checked. *)
    Not_checked
  | [ only ] ->
    if check1 only
    then Valid
    else if Option.equal Int.equal only.exception_ (Some 14)
    then
      (* Exception 14: if h is 0, 1 or 9, drop it, shift the account number right one
         place and check again. *)
      if d.(h) = 0 || d.(h) = 1 || d.(h) = 9
      then
        of_bool
          (run_check only (digits ~sort_code:sc ~account:("0" ^ String.prefix account 7)))
      else Invalid
    else Invalid
  | first :: second :: _ ->
    let ex (r : Rule.t) = r.exception_ in
    (match ex first, ex second with
     | Some 2, Some 9 ->
       (* The second check runs only if the first fails, on sort code 309634. *)
       of_bool (check1 first || run_check second (digits ~sort_code:"309634" ~account))
     | Some 10, Some 11 | Some 12, Some 13 -> of_bool (check1 first || check1 second)
     | _, Some 3 when d.(c) = 6 || d.(c) = 9 -> of_bool (check1 first)
     | _ -> of_bool (check1 first && check1 second))
;;
