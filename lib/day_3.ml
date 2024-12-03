let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum
let my_chars_to_int ls = Batteries.String.of_list ls |> int_of_string
let print_int_tuples t = Printf.printf "(%d, %d)\n" (fst t) (snd t)
let calc_results ls = List.fold_left (fun acc (l, r) -> acc + (l * r)) 0 ls

let rec find_mul = function
  | [] -> None
  | 'm' :: 'u' :: 'l' :: '(' :: tl -> Some tl
  | _ :: tl -> find_mul tl
;;

let rec find_next acc end_ch = function
  | [] -> None
  | _ when List.length acc > 3 -> None
  | ('0' .. '9' as c) :: tl -> find_next (c :: acc) end_ch tl
  | ch :: tl when ch = end_ch ->
    let v = List.rev acc |> my_chars_to_int in
    Some (v, tl)
  | _ -> None
;;

let rec find_mul_v2 state ls =
  if state = true
  then (
    match ls with
    | [] -> None
    | 'd' :: 'o' :: 'n' :: '\'' :: 't' :: '(' :: ')' :: tl -> find_mul_v2 false tl
    | 'm' :: 'u' :: 'l' :: '(' :: tl -> Some tl
    | _ :: tl -> find_mul_v2 true tl)
  else (
    match ls with
    | [] -> None
    | 'd' :: 'o' :: '(' :: ')' :: tl -> find_mul_v2 true tl
    | _ :: tl -> find_mul_v2 false tl)
;;

let parse_v2 ls =
  let rec aux acc ls =
    match find_mul_v2 true ls with
    | None -> acc
    | Some ls ->
      (match find_next [] ',' ls with
       | None -> aux acc ls
       | Some (l, ls) ->
         (match find_next [] ')' ls with
          | None -> aux acc ls
          | Some (r, ls) -> aux ((l, r) :: acc) ls))
  in
  aux [] ls
;;

let parse ls =
  let rec aux acc ls =
    match find_mul ls with
    | None -> acc
    | Some ls ->
      (match find_next [] ',' ls with
       | None -> aux acc ls
       | Some (l, ls) ->
         (match find_next [] ')' ls with
          | None -> aux acc ls
          | Some (r, ls) -> aux ((l, r) :: acc) ls))
  in
  aux [] ls
;;

let part_a fname =
  let ls = read_file fname |> List.fold_left ( ^ ) "" |> Batteries.String.explode in
  let filtered = parse ls in
  calc_results filtered
;;

let part_b fname =
  let ls = read_file fname |> List.fold_left ( ^ ) "" |> Batteries.String.explode in
  let filtered = parse_v2 ls in
  calc_results filtered
;;
