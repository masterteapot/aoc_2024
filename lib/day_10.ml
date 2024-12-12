let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum

type direction =
  | Up
  | Down
  | Left
  | Right

let add_tuple l r = fst l + fst r, snd l + snd r

let calc_move pos = function
  | Up -> add_tuple pos (0, -1)
  | Down -> add_tuple pos (0, 1)
  | Left -> add_tuple pos (-1, 0)
  | Right -> add_tuple pos (1, 0)
;;

let get_val arr pos = arr.(snd pos).(fst pos)

let can_move arr pos dir =
  try
    let gv = get_val arr in
    let new_val = gv (calc_move pos dir) in
    let curr_val = gv pos in
    if new_val = curr_val + 1 then true else false
  with
  | _ -> false
;;

let print_int_arr_arr arr =
  Array.iter
    (fun a ->
       print_string "[| ";
       Array.iter (Printf.printf "%d; ") a;
       print_endline "|]")
    arr
;;

let find_zeros arr =
  assert (Array.length arr > 0);
  assert (Array.length arr.(0) > 0);
  let max_x = Array.length arr.(0) in
  let max_y = Array.length arr in
  let rec aux x y acc =
    if y = max_y
    then acc
    else if x = max_x
    then aux 0 (y + 1) acc
    else (
      match arr.(y).(x) with
      | 0 -> aux (x + 1) y ((x, y) :: acc)
      | _ -> aux (x + 1) y acc)
  in
  aux 0 0 []
;;

let walk arr =
  let zeros = find_zeros arr in
  let rec aux loc =
    let gv = get_val arr in
    let calc = calc_move loc in
    let can = can_move arr loc in
    if gv loc = 9
    then [ loc ]
    else
      (if can Up then aux (calc Up) else [])
      @ (if can Down then aux (calc Down) else [])
      @ (if can Left then aux (calc Left) else [])
      @ if can Right then aux (calc Right) else []
  in
  List.map aux zeros
;;

let part_a fname =
  let raw_inputs = read_file fname in
  let arr =
    List.map Batteries.String.explode raw_inputs
    |> List.map (fun l -> List.map (fun x -> int_of_string (String.make 1 x)) l)
    |> List.map Array.of_list
    |> Array.of_list
  in
  print_int_arr_arr arr;
  let trail_heads = walk arr in
  let results = List.map Batteries.List.unique trail_heads |> List.map List.length in
  print_endline "";
  List.iter (Printf.printf "%d; ") results;
  print_endline "";
  List.fold_left ( + ) 0 results
;;

let part_b fname =
  let raw_inputs = read_file fname in
  let arr =
    List.map Batteries.String.explode raw_inputs
    |> List.map (fun l -> List.map (fun x -> int_of_string (String.make 1 x)) l)
    |> List.map Array.of_list
    |> Array.of_list
  in
  print_int_arr_arr arr;
  let trail_heads = walk arr in
  let results = List.map List.length trail_heads in
  print_endline "";
  List.iter (Printf.printf "%d; ") results;
  print_endline "";
  List.fold_left ( + ) 0 results
;;
