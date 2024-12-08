module Hash = struct
  include Batteries.Hashtbl
end

type direction =
  | N
  | S
  | E
  | W

type location =
  | Obsticle
  | Blank
  | Visited of direction list

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
  | Visited [ E ] | Visited [ W ] | Visited [ E; W ] | Visited [ W; E ] -> "-"
  | Visited [ N ] | Visited [ S ] | Visited [ N; S ] | Visited [ S; N ] -> "|"
  | Visited _ -> "+"
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

let rec run_routes tbl g =
  let rec_routes = run_routes tbl in
  try
    match Hash.find tbl g.position with
    | Obsticle -> rec_routes (go_back_and_turn g)
    | Blank ->
      Hash.modify g.position (fun _ -> Visited [ g.dir ]) tbl;
      rec_routes (move_guard g)
    | Visited l ->
      Hash.modify g.position (fun _ -> Visited (g.dir :: l)) tbl;
      rec_routes (move_guard g)
  with
  | _ -> ()
;;

let rec is_loop tbl g =
  let rec_routes = is_loop tbl in
  try
    match Hash.find tbl g.position with
    | Obsticle -> rec_routes (go_back_and_turn g)
    | Blank ->
      Hash.modify g.position (fun _ -> Visited [ g.dir ]) tbl;
      rec_routes (move_guard g)
    | Visited l when List.mem g.dir l -> true
    | Visited l ->
      Hash.modify g.position (fun _ -> Visited (g.dir :: l)) tbl;
      rec_routes (move_guard g)
  with
  | _ -> false
;;

let rec run_routes_v2 tbl g acc =
  let rec_routes_v2 = run_routes_v2 tbl in
  try
    match Hash.find tbl g.position with
    | Obsticle -> rec_routes_v2 (go_back_and_turn g) acc
    | Blank ->
      let tbl_copy = Hash.copy tbl in
      if
        Hash.mem tbl (move_guard g).position
        && Hash.find tbl (move_guard g).position <> Obsticle
        &&
        (Hash.modify (move_guard g).position (fun _ -> Obsticle) tbl_copy;
         is_loop tbl_copy g)
      then (
        Hash.modify g.position (fun _ -> Visited [ g.dir ]) tbl;
        rec_routes_v2 (move_guard g) ((move_guard g).position :: acc))
      else (
        Hash.modify g.position (fun _ -> Visited [ g.dir ]) tbl;
        rec_routes_v2 (move_guard g) acc)
    | Visited l ->
      let tbl_copy = Hash.copy tbl in
      if
        Hash.mem tbl (move_guard g).position
        && Hash.find tbl (move_guard g).position <> Obsticle
        &&
        (Hash.modify (move_guard g).position (fun _ -> Obsticle) tbl_copy;
         is_loop tbl_copy g)
      then (
        Hash.modify g.position (fun _ -> Visited (g.dir :: l)) tbl;
        rec_routes_v2 (move_guard g) ((move_guard g).position :: acc))
      else (
        Hash.modify g.position (fun _ -> Visited (g.dir :: l)) tbl;
        rec_routes_v2 (move_guard g) acc)
  with
  | _ -> acc
;;

let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum
let print_tuple (x, y) = Printf.printf "%d, %d\n" x y

let part_a fname =
  let raw_inputs = read_file fname in
  let char_ls = List.map Batteries.String.explode raw_inputs in
  let tbl = Hash.create (List.length char_ls * List.length (List.hd char_ls)) in
  parse_map char_ls tbl;
  let guard = find_guard char_ls in
  (* let keys = Hash.keys tbl |> Batteries.List.of_enum in *)
  (* let srted_keys = List.sort sort_coords keys in *)
  run_routes tbl guard;
  print_endline "";
  (* print_tbl srted_keys tbl guard; *)
  Hash.fold
    (fun _ v a ->
       match v with
       | Visited _ -> a + 1
       | _ -> a)
    tbl
    0
;;

let part_b fname =
  let raw_inputs = read_file fname in
  let char_ls = List.map Batteries.String.explode raw_inputs in
  let tbl = Hash.create (List.length char_ls * List.length (List.hd char_ls)) in
  parse_map char_ls tbl;
  let guard = find_guard char_ls in
  (* let keys = Hash.keys tbl |> Batteries.List.of_enum in *)
  (* let srted_keys = List.sort sort_coords keys in *)
  let obsticles =
    run_routes_v2 tbl guard []
    |> Batteries.List.unique
    |> List.filter (fun x -> x <> guard.position)
  in
  print_endline "";
  (* print_tbl srted_keys tbl guard; *)
  (* List.iter print_tuple obsticles; *)
  List.length obsticles
;;
