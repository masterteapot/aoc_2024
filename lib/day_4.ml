let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum
let sum_int_list ls = List.fold_left ( + ) 0 ls

let print_type_arr_arr arr inner_printf_str =
  print_endline "[|";
  Array.iter
    (fun y ->
       print_string "[|";
       Array.iter (fun x -> Printf.printf inner_printf_str x) y;
       print_endline "|]")
    arr;
  print_endline "|]"
;;

let get_arr_val arr loc = arr.(snd loc).(fst loc)
let get_next_loc loc dir = fst loc + fst dir, snd loc + snd dir

let get_xmas arr loc dir =
  let loc1 = loc in
  let loc2 = get_next_loc loc dir in
  let loc3 = get_next_loc loc2 dir in
  let loc4 = get_next_loc loc3 dir in
  try
    if
      get_arr_val arr loc1 = 'X'
      && get_arr_val arr loc2 = 'M'
      && get_arr_val arr loc3 = 'A'
      && get_arr_val arr loc4 = 'S'
    then 1
    else 0
  with
  | _ -> 0
;;

let count_xmas arr loc =
  let dir_calc = get_xmas arr loc in
  dir_calc (0, -1)
  + dir_calc (1, -1)
  + dir_calc (1, 0)
  + dir_calc (1, 1)
  + dir_calc (0, 1)
  + dir_calc (-1, 1)
  + dir_calc (-1, 0)
  + dir_calc (-1, -1)
;;

let map_solutions arr out =
  Array.iteri
    (fun yi ya ->
       Array.iteri
         (fun xi _ ->
            let res =
              match arr.(yi).(xi) with
              | 'X' -> count_xmas arr (xi, yi)
              | _ -> 0
            in
            out.(yi).(xi) <- res)
         ya)
    arr
;;

let count_xmas_v2 arr loc =
  let curr = get_arr_val arr loc in
  assert (curr = 'A');
  let get_next_val dir = get_next_loc loc dir |> get_arr_val arr in
  try
    let ul = get_next_val (-1, -1) in
    let ur = get_next_val (1, -1) in
    let dl = get_next_val (-1, 1) in
    let dr = get_next_val (1, 1) in
    match (ul, dr), (ur, dl) with
    | ('S', 'M'), ('S', 'M')
    | ('M', 'S'), ('S', 'M')
    | ('M', 'S'), ('M', 'S')
    | ('S', 'M'), ('M', 'S') -> 1
    | _ -> 0
  with
  | _ -> 0
;;

let map_solutions_v2 arr out =
  Array.iteri
    (fun yi ya ->
       Array.iteri
         (fun xi _ ->
            let res =
              match arr.(yi).(xi) with
              | 'A' -> count_xmas_v2 arr (xi, yi)
              | _ -> 0
            in
            out.(yi).(xi) <- res)
         ya)
    arr
;;

let part_a fname =
  let ls =
    read_file fname
    |> List.map Batteries.String.explode
    |> List.map Batteries.Array.of_list
    |> Batteries.Array.of_list
  in
  let res_arr =
    Batteries.Array.init (Batteries.Array.length ls) (fun _ ->
      Batteries.Array.init (Batteries.Array.length ls.(0)) (fun _ -> 0))
  in
  let _ = map_solutions ls res_arr in
  Array.fold_left (fun acc ya -> acc + Array.fold_left ( + ) 0 ya) 0 res_arr
;;

let part_b fname =
  let ls =
    read_file fname
    |> List.map Batteries.String.explode
    |> List.map Batteries.Array.of_list
    |> Batteries.Array.of_list
  in
  let res_arr =
    Batteries.Array.init (Batteries.Array.length ls) (fun _ ->
      Batteries.Array.init (Batteries.Array.length ls.(0)) (fun _ -> 0))
  in
  let _ = map_solutions_v2 ls res_arr in
  Array.fold_left (fun acc ya -> acc + Array.fold_left ( + ) 0 ya) 0 res_arr
;;
