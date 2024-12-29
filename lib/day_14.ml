let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum

type robot =
  { pos : int * int
  ; vel : int * int
  }

let double_at ls n1 n2 =
  let inner_ls = List.nth ls n1 in
  List.nth inner_ls n2
;;

let wrap_robot max_val v =
  if v < 0 then max_val + v + 1 else if v > max_val then v - 1 - max_val else v
;;

let print_robot r =
  Printf.printf
    "{pos: %d, %d; vel: %d, %d}\n"
    (fst r.pos)
    (snd r.pos)
    (fst r.vel)
    (snd r.vel)
;;

let create_range bottom top =
  assert (bottom < top);
  let rec aux n acc = if n < bottom then acc else aux (n - 1) (n :: acc) in
  aux top []
;;

let move_robot (max_x, max_y) num r =
  let rem_x = Int.rem (fst r.vel * num) (max_x + 1) in
  let rem_y = Int.rem (snd r.vel * num) (max_y + 1) in
  let new_x = fst r.pos + rem_x |> wrap_robot max_x in
  let new_y = snd r.pos + rem_y |> wrap_robot max_y in
  { r with pos = new_x, new_y }
;;

let parse_robot str =
  let split = String.split_on_char ' ' str |> List.map (Batteries.String.lchop ~n:2) in
  let split = List.map (String.split_on_char ',') split in
  { pos = int_of_string @@ double_at split 0 0, int_of_string @@ double_at split 0 1
  ; vel = int_of_string @@ double_at split 1 0, int_of_string @@ double_at split 1 1
  }
;;

let score_quadrants w h lr =
  let rec aux tl tr bl br = function
    | [] -> tl * tr * bl * br
    | r :: ls when (fst r.pos * 2) + 1 = w || (snd r.pos * 2) + 1 = h ->
      aux tl tr bl br ls
    | r :: ls when fst r.pos * 2 < w && snd r.pos * 2 < h -> aux (1 + tl) tr bl br ls
    | r :: ls when fst r.pos * 2 > w && snd r.pos * 2 < h -> aux tl (1 + tr) bl br ls
    | r :: ls when fst r.pos * 2 < w && snd r.pos * 2 > h -> aux tl tr (1 + bl) br ls
    | r :: ls when fst r.pos * 2 > w && snd r.pos * 2 > h -> aux tl tr bl (1 + br) ls
    | _ -> failwith "I missed a potential case"
  in
  aux 0 0 0 0 lr
;;

let num_diagonal x y arr =
  let aux dx dy =
    try
      match arr.(dy).(dx) with
      | 0 -> 0
      | _ -> 1
    with
    | _ -> 0
  in
  aux (x + 1) (y + 1) + aux (x - 1) (y + 1) + aux (x + 1) (y - 1) + aux (x - 1) (y - 1)
;;

let is_xmax_tree match_target num_robots arr =
  let max_x = Array.length arr.(0) in
  let max_y = Array.length arr in
  let rec aux x y num_matches =
    if y = max_y
    then num_matches
    else if x = max_x
    then aux 0 (y + 1) num_matches
    else (
      match arr.(y).(x) with
      | 0 -> aux (x + 1) y num_matches
      | n ->
        (match num_diagonal x y arr with
         | 0 | 1 -> aux (x + 1) y num_matches
         | _ -> aux (x + 1) y (num_matches + n)))
  in
  let matches = aux 0 0 0 in
  let match_rate = float_of_int matches /. float_of_int num_robots in
  match_rate > match_target
;;

let zero_out arr max_x max_y v =
  let rec aux x y =
    if y = max_y
    then ()
    else if x = max_x
    then aux 0 (y + 1)
    else (
      arr.(y).(x) <- v;
      aux (x + 1) y)
  in
  aux 0 0
;;

let rec update_robots arr = function
  | [] -> ()
  | r :: tl ->
    arr.(snd r.pos).(fst r.pos) <- 1 + arr.(snd r.pos).(fst r.pos);
    update_robots arr tl
;;

let print_xmas_tree arr =
  Array.iter
    (fun y ->
       print_string "[|";
       Array.iter (fun x -> if x = 0 then print_string "." else print_string "#") y;
       print_endline "|]")
    arr
;;

let loop_robots (x_length, y_length) robots match_target =
  let num_robots = List.length robots in
  let bot_array = Array.make_matrix y_length x_length 0 in
  let rec aux counter robots =
    zero_out bot_array x_length y_length 0;
    let robots = List.map (move_robot (100, 102) 1) robots in
    update_robots bot_array robots;
    match is_xmax_tree match_target num_robots bot_array with
    | false -> aux (counter + 1) robots
    | true ->
      print_endline (string_of_int counter);
      print_xmas_tree bot_array;
      counter
  in
  aux 1 robots
;;

let part_a fname =
  let raw_inputs = read_file fname in
  let inputs = List.map parse_robot raw_inputs in
  let output = List.map (move_robot (100, 102) 100) inputs in
  score_quadrants 101 103 output
;;

let part_b fname =
  let raw_inputs = read_file fname in
  let inputs = List.map parse_robot raw_inputs in
  let array_dimensions = 101, 103 in
  let output = loop_robots array_dimensions inputs 0.40 in
  output
;;
