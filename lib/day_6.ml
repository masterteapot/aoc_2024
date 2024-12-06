module Hash = struct
  include Batteries.Hashtbl
end

type location =
  | Obsticle
  | Visited of int

type direction =
  | N
  | S
  | E
  | W

type guard =
  { position : int * int
  ; dir : direction
  }

let get_locs = function
  | '#' -> Obsticle
  | '.' -> Visited 0
  | '^' | '>' | '<' | 'V' | 'v' -> Visited 0
  | c -> failwith (Printf.sprintf "get_locs did not expect this char [%c]" c)
;;

let print_locs = function
  | Obsticle -> "#"
  | Visited d -> string_of_int d
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

type ways =
  | Left
  | Right

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

let rec run_routes g tbl =
  try
    match Hash.find tbl g.position with
    | Obsticle -> run_routes (go_back_and_turn g) tbl
    | Visited _ ->
      Hash.modify
        g.position
        (fun x ->
           match x with
           | Visited n -> Visited (n + 1)
           | _ -> failwith "should have visited")
        tbl;
      run_routes (move_guard g) tbl
  with
  | _ -> ()
;;

let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum

let part_a fname =
  let raw_inputs = read_file fname in
  let char_ls = List.map Batteries.String.explode raw_inputs in
  let tbl = Hash.create (List.length char_ls * List.length (List.hd char_ls)) in
  parse_map char_ls tbl;
  List.iter
    (fun l ->
       List.iter print_char l;
       print_newline ())
    char_ls;
  let guard = find_guard char_ls in
  run_routes guard tbl;
  (* Hash.iter (fun (x, y) v -> Printf.printf "%d, %d -> %s\n" x y @@ print_locs v) tbl; *)
  Hash.fold
    (fun _ v a ->
       match v with
       | Visited n -> if n > 0 then a + 1 else a
       | _ -> a)
    tbl
    0
;;

let part_b _ = 0
