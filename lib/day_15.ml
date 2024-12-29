type direction =
  | Up
  | Down
  | Right
  | Left

type map_char =
  | Wall
  | Box
  | Empty
  | Robot

type big_map_object =
  | BWall
  | BoxL
  | BoxR
  | BEmpty
  | BRobot

let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum

let split_list x ls =
  let rec aux acc = function
    | [] -> failwith "didn't find split val"
    | hd :: tl when hd = x -> List.rev acc, tl
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux [] ls
;;

let char_to_map_char = function
  | '#' -> Wall
  | '@' -> Robot
  | 'O' -> Box
  | '.' -> Empty
  | _ -> failwith "invalid char for map char"
;;

let map_char_to_string = function
  | Wall -> "#"
  | Robot -> "@"
  | Box -> "O"
  | Empty -> "."
;;

let big_map_object_to_string = function
  | BWall -> "#"
  | BRobot -> "@"
  | BoxL -> "["
  | BoxR -> "]"
  | BEmpty -> "."
;;

let print_map arr =
  Array.iter
    (fun y ->
       print_string "[|";
       Array.iter (fun x -> print_string @@ map_char_to_string x) y;
       print_endline "|]")
    arr
;;

let print_map_v2 arr =
  Array.iter
    (fun y ->
       print_string "[|";
       Array.iter (fun x -> print_string @@ big_map_object_to_string x) y;
       print_endline "|]")
    arr
;;

let parse_input ls =
  let raw_map, raw_input = split_list "" ls in
  let map =
    raw_map
    |> List.map Batteries.String.explode
    |> List.map Array.of_list
    |> Array.of_list
    |> Array.map (Array.map char_to_map_char)
  in
  let directions =
    raw_input
    |> List.fold_left ( ^ ) ""
    |> Batteries.String.explode
    |> List.map (fun c ->
      match c with
      | '^' -> Up
      | '>' -> Right
      | '<' -> Left
      | 'v' -> Down
      | _ -> failwith "unrecognized direction")
  in
  map, directions
;;

let parse_input_v2 ls =
  let raw_map, raw_input = split_list "" ls in
  let map =
    raw_map
    |> List.map Batteries.String.explode
    |> List.map Array.of_list
    |> Array.of_list
    |> Array.map (fun a ->
      let na = Array.make (Array.length a * 2) BEmpty in
      Array.iteri
        (fun i x ->
           match x with
           | '#' ->
             na.(i * 2) <- BWall;
             na.((i * 2) + 1) <- BWall
           | 'O' ->
             na.(i * 2) <- BoxL;
             na.((i * 2) + 1) <- BoxR
           | '.' ->
             na.(i * 2) <- BEmpty;
             na.((i * 2) + 1) <- BEmpty
           | '@' ->
             na.(i * 2) <- BRobot;
             na.((i * 2) + 1) <- BEmpty
           | _ -> failwith "unrecognized big map char")
        a;
      na)
  in
  let directions =
    raw_input
    |> List.fold_left ( ^ ) ""
    |> Batteries.String.explode
    |> List.map (fun c ->
      match c with
      | '^' -> Up
      | '>' -> Right
      | '<' -> Left
      | 'v' -> Down
      | _ -> failwith "unrecognized direction")
  in
  map, directions
;;

let find_arr_arr v arr =
  let max_y = Array.length arr in
  let max_x = Array.length arr.(0) in
  let rec aux x y =
    if y = max_y
    then failwith "didn't find value"
    else if x = max_x
    then aux 0 (y + 1)
    else if arr.(y).(x) = v
    then x, y
    else aux (x + 1) y
  in
  aux 0 0
;;

let get_val arr coord = arr.(snd coord).(fst coord)

let move (x, y) = function
  | Up -> x, y - 1
  | Down -> x, y + 1
  | Left -> x - 1, y
  | Right -> x + 1, y
;;

let dir_to_string = function
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
;;

let gps arr =
  let max_y = Array.length arr in
  let max_x = Array.length arr in
  let rec aux x y acc =
    if y = max_y
    then acc
    else if x = max_x
    then aux 0 (y + 1) acc
    else (
      match arr.(y).(x) with
      | Box ->
        let nacc = acc + x + (y * 100) in
        aux (x + 1) y nacc
      | _ -> aux (x + 1) y acc)
  in
  aux 0 0 0
;;

let gps_v2 arr =
  let max_y = Array.length arr in
  let max_x = Array.length arr in
  let rec aux x y acc =
    if y = max_y
    then acc
    else if x = max_x
    then aux 0 (y + 1) acc
    else (
      match arr.(y).(x) with
      | BoxL ->
        let nacc = acc + x + (y * 100) in
        aux (x + 1) y nacc
      | _ -> aux (x + 1) y acc)
  in
  aux 0 0 0
;;

let walk_map arr dirs =
  let coord = find_arr_arr Robot arr in
  let gv = get_val arr in
  let rec move_things dir = function
    | [] -> ()
    | (x, y) :: tl ->
      let nx, ny = move (x, y) dir in
      arr.(ny).(nx) <- arr.(y).(x);
      arr.(y).(x) <- Empty;
      move_things dir tl
  in
  let rec aux acc robot coord = function
    | [] -> arr
    | d :: tl when gv coord = Empty ->
      move_things d acc;
      aux [] (move robot d) (move robot d) tl
    | _ :: tl when gv coord = Wall -> aux [] robot robot tl
    | d :: _ as ls -> aux (coord :: acc) robot (move coord d) ls
  in
  aux [] coord coord dirs
;;

let find_moveable_boxes arr (x, y) dir =
  let gv = get_val arr in
  let rec aux acc coord =
    match gv coord with
    | BEmpty -> acc
    | BWall -> []
    | BoxL when dir = Up || dir = Down ->
      let left = aux (coord :: acc) (move coord dir) in
      let right = if List.length acc = 0 then [] else aux [] (move coord Right) in
      if List.length left = 0 || List.length right = 0
      then []
      else right @ left |> Batteries.List.unique
    | BoxR when dir = Up || dir = Down ->
      let left = aux (coord :: acc) (move coord dir) in
      let right = if List.length acc = 0 then [] else aux [] (move coord Left) in
      if List.length left = 0 || List.length right = 0
      then []
      else right @ left |> Batteries.List.unique
    | BRobot -> aux (coord :: acc) (move coord dir)
    | _ -> aux (coord :: acc) (move coord dir)
  in
  assert (gv (x, y) = BRobot);
  aux [] (x, y)
;;

let walk_map_v2 arr dirs =
  let coord = find_arr_arr BRobot arr in
  let rec move_things dir = function
    | [] -> ()
    | (x, y) :: tl ->
      let nx, ny = move (x, y) dir in
      arr.(ny).(nx) <- arr.(y).(x);
      arr.(y).(x) <- BEmpty;
      move_things dir tl
  in
  let rec aux robot = function
    | [] -> ()
    | d :: tl ->
      (match find_moveable_boxes arr robot d with
       | [] -> aux robot tl
       | ls ->
         move_things d ls;
         Printf.printf "Moving: %s\n" (dir_to_string d);
         print_map_v2 arr;
         aux (move robot d) tl)
  in
  aux coord dirs
;;

let part_a fname =
  let arr, directions = read_file fname |> parse_input in
  let arr = walk_map arr directions in
  gps arr
;;

let part_b fname =
  let arr, directions = read_file fname |> parse_input_v2 in
  walk_map_v2 arr directions;
  print_newline ();
  print_map_v2 arr;
  gps_v2 arr
;;
