module Hash = struct
  include Batteries.Hashtbl
end

type location =
  | Obsticle
  | Blank
  | Visited of char

type direction =
  | N
  | S
  | E
  | W

type ways =
  | Left
  | Right

type guard =
  { position : int * int
  ; dir : direction
  }

let get_locs = function
  | '#' -> Obsticle
  | _ -> Blank
;;

let print_locs = function
  | Obsticle -> "#"
  | Blank -> "."
  | Visited '-' -> "-"
  | Visited '|' -> "|"
  | Visited '+' -> "+"
  | Visited c -> failwith (Printf.sprintf "unexpected print_locs value: %c" c)
;;

let clear_screen =
  Printf.printf "\033c";
  flush stdout
;;

let parse_map ls tbl =
  let rec aux counter = function
    | [] -> ()
    | hd :: tl ->
      List.iteri (fun i x -> Hash.add tbl (i, counter) (get_locs x)) hd;
      aux (counter + 1) tl
  in
  aux 0 ls
;;

let is_guard = function
  | '^' -> Some N
  | '>' -> Some E
  | '<' -> Some W
  | 'V' | 'v' -> Some S
  | _ -> None
;;

let dir_to_string = function
  | N -> "North"
  | E -> "East"
  | S -> "South"
  | W -> "West"
;;

let dir_to_char c = function
  | N when c = '-' -> '+'
  | S when c = '-' -> '+'
  | E when c = '|' -> '+'
  | W when c = '|' -> '+'
  | N | S -> '|'
  | E | W -> '-'
;;

let find_guard ls =
  let rec aux counter = function
    | [] -> failwith "we did not find a guard"
    | hd :: tl ->
      (match
         Batteries.List.fold_lefti
           (fun acc i x ->
              match is_guard x with
              | None -> acc
              | Some v -> i, Some v)
           (0, None)
           hd
       with
       | _, None -> aux (counter + 1) tl
       | x, Some d -> { position = x, counter; dir = d })
  in
  aux 0 ls
;;

let turn_guard g way =
  let get_new_dir = function
    | Left ->
      (match g.dir with
       | N -> W
       | W -> S
       | S -> E
       | E -> N)
    | Right ->
      (match g.dir with
       | N -> E
       | E -> S
       | S -> W
       | W -> N)
  in
  let new_dir = get_new_dir way in
  { g with dir = new_dir }
;;

let add_tuples l r = fst l + fst r, snd l + snd r

let move_guard g =
  let movement =
    match g.dir with
    | N -> 0, -1
    | E -> 1, 0
    | S -> 0, 1
    | W -> -1, 0
  in
  { g with position = add_tuples g.position movement }
;;

let move_back g =
  let movement =
    match g.dir with
    | N -> 0, 1
    | E -> -1, 0
    | S -> 0, -1
    | W -> 1, 0
  in
  { g with position = add_tuples g.position movement }
;;

let go_back_and_turn g =
  let backed_up = move_back g in
  turn_guard backed_up Right
;;

let print_guard g =
  Printf.printf "(%d, %d -> %s)\n" (fst g.position) (snd g.position) (dir_to_string g.dir)
;;

let rec print_tbl srted_keys tbl g =
  match srted_keys with
  | [] ->
    print_endline "";
    print_endline "";
    flush stdout
  | ((_, hy) as hd) :: ((_, my) :: _ as ls) when my > hy ->
    if hd = g.position
    then (
      print_string "G";
      print_endline "";
      print_tbl ls tbl g)
    else (
      print_string @@ print_locs @@ Hash.find tbl hd;
      print_newline ();
      print_tbl ls tbl g)
  | hd :: tl when hd = g.position ->
    print_string "G";
    print_tbl tl tbl g
  | hd :: tl when hd = g.position ->
    print_string "G";
    print_tbl tl tbl g
  | hd :: tl ->
    print_string @@ print_locs @@ Hash.find tbl hd;
    flush stdout;
    print_tbl tl tbl g
;;

let sort_coords l r =
  if snd l > snd r
  then 1
  else if snd l < snd r
  then -1
  else if fst l > fst r
  then 1
  else if fst l < fst r
  then -1
  else 0
;;

let rec run_routes srted_keys tbl g =
  let rec_routes = run_routes srted_keys tbl in
  try
    match Hash.find tbl g.position with
    | Obsticle -> rec_routes (go_back_and_turn g)
    | Blank ->
      Hash.modify g.position (fun _ -> Visited (dir_to_char '.' g.dir)) tbl;
      rec_routes (move_guard g)
    | Visited c ->
      Hash.modify g.position (fun _ -> Visited (dir_to_char c g.dir)) tbl;
      rec_routes (move_guard g)
  with
  | _ -> ()
;;

let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum

let part_a fname =
  let raw_inputs = read_file fname in
  let char_ls = List.map Batteries.String.explode raw_inputs in
  let tbl = Hash.create (List.length char_ls * List.length (List.hd char_ls)) in
  parse_map char_ls tbl;
  let guard = find_guard char_ls in
  let keys = Hash.keys tbl |> Batteries.List.of_enum in
  let srted_keys = List.sort sort_coords keys in
  run_routes srted_keys tbl guard;
  print_tbl srted_keys tbl guard;
  Hash.fold
    (fun _ v a ->
       match v with
       | Visited _ -> a + 1
       | _ -> a)
    tbl
    0
;;

let part_b _ = 0
