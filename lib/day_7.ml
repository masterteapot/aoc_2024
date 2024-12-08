let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum
let print_tuple (x, y) = Printf.printf "%d, %d\n" x y

let parse_part_a ls =
  assert (List.length ls = 2);
  let left = int_of_string @@ List.hd ls in
  let right =
    List.nth ls 1 |> String.trim |> String.split_on_char ' ' |> List.map int_of_string
  in
  left, right
;;

let print_input x =
  Printf.printf "%d: " (fst x);
  List.iter (fun i -> Printf.printf "%d, " i) (snd x);
  print_endline ""
;;

let evaluate (v, ls) =
  let rec aux acc = function
    | [] -> acc = v
    | hd :: tl when acc = 0 -> aux hd tl || aux hd tl
    | hd :: tl -> aux (hd + acc) tl || aux (hd * acc) tl
  in
  aux 0 ls
;;

let evaluate_v2 (v, k) =
  let rec aux acc ls =
    let acc_value =
      int_of_string (List.fold_left (fun a n -> string_of_int n ^ a) "" acc)
    in
    print_endline (string_of_int acc_value);
    if acc_value > v
    then (
      print_newline ();
      false)
    else (
      match ls with
      | [] -> acc_value = v
      | hd :: tl ->
        (match acc with
         | [] -> failwith "shouldn't be empty"
         | ahd :: atl -> aux ((hd + ahd) :: atl) tl || aux ((hd * ahd) :: atl) tl)
        || aux (hd :: acc) tl)
  in
  assert (List.length k > 1);
  aux [ List.hd k ] (List.tl k)
;;

let evaluate_v3 (v, k) =
  let rec aux acc ls =
    if acc > v
    then false
    else (
      match ls with
      | [] -> acc = v
      | hd :: tl ->
        aux (hd + acc) tl
        || aux (hd * acc) tl
        || aux (int_of_string (string_of_int acc ^ string_of_int hd)) tl)
  in
  assert (List.length k > 1);
  aux (List.hd k) (List.tl k)
;;

let part_a fname =
  let raw_inputs = read_file fname in
  let inputs =
    raw_inputs |> List.map (String.split_on_char ':') |> List.map parse_part_a
  in
  let output = List.filter evaluate inputs in
  List.fold_left (fun a x -> fst x + a) 0 output
;;

let part_b fname =
  let raw_inputs = read_file fname in
  let inputs =
    raw_inputs |> List.map (String.split_on_char ':') |> List.map parse_part_a
  in
  let output = List.filter evaluate_v3 inputs in
  Printf.printf "Num Results: %d\n" (List.length output);
  List.fold_left (fun a x -> fst x + a) 0 output
;;
