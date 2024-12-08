module Hash = struct
  include Batteries.Hashtbl
end

type ants =
  | Antenna of char
  | Blank
  | Antinode
  | Mix of char

let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum
let print_tuple (x, y) = Printf.printf "%d, %d\n" x y

let get_locs = function
  | '0' .. '9' as c -> Antenna c
  | 'a' .. 'z' as c -> Antenna c
  | 'A' .. 'Z' as c -> Antenna c
  | '.' -> Blank
  | _ -> failwith "unexpected starting character"
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

let rec add_ants coords_tbl ant_tbl = function
  | [] -> ()
  | hd :: tl ->
    (match Hash.find coords_tbl hd with
     | Blank | Mix _ | Antinode -> add_ants coords_tbl ant_tbl tl
     | Antenna c ->
       if Hash.mem ant_tbl c
       then (
         Hash.modify c (fun ls -> hd :: ls) ant_tbl;
         add_ants coords_tbl ant_tbl tl)
       else (
         Hash.add ant_tbl c [ hd ];
         add_ants coords_tbl ant_tbl tl))
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

let move_coords f l r = f (fst l) (fst r), f (snd l) (snd r)

let create_antinode ls =
  let rec aux acc = function
    | [] -> acc
    | [ _ ] -> acc
    | hd :: md :: tl ->
      let dif = move_coords ( - ) hd md in
      let bottom = move_coords ( + ) hd dif in
      let top = move_coords ( - ) md dif in
      let acc = aux (bottom :: top :: acc) (hd :: tl) in
      aux acc (md :: tl)
  in
  aux [] ls
;;

let create_antinode_v2 max_x max_y ls =
  let is_valid_loc loc =
    fst loc >= 0 && snd loc >= 0 && fst loc <= max_x && snd loc <= max_y
  in
  let rec all_locs f delta acc v =
    let new_v = move_coords f v delta in
    if is_valid_loc new_v then all_locs f delta (new_v :: acc) new_v else acc
  in
  let rec aux acc = function
    | [] -> acc
    | [ _ ] -> acc
    | hd :: md :: tl ->
      let dif = move_coords ( - ) hd md in
      let bottom_locs = all_locs ( - ) dif [] hd in
      let top_locs = all_locs ( + ) dif [] hd in
      let acc = aux ((hd :: bottom_locs) @ top_locs @ acc) (hd :: tl) in
      aux acc (md :: tl)
  in
  aux [] ls
;;

let print_ants k v =
  Printf.printf "%c: [" k;
  List.iter (fun (x, y) -> Printf.printf "(%d, %d); " x y) v;
  print_endline "]"
;;

let add_antinode coords_tbl antinode =
  if Hash.mem coords_tbl antinode
  then (
    match Hash.find coords_tbl antinode with
    | Antenna c -> Hash.modify antinode (fun _ -> Mix c) coords_tbl
    | Antinode | Mix _ -> ()
    | Blank -> Hash.modify antinode (fun _ -> Antinode) coords_tbl)
  else ()
;;

let part_a fname =
  let raw_inputs = read_file fname in
  let char_ls = List.map Batteries.String.explode raw_inputs in
  let coords_tbl = Hash.create (List.length char_ls * List.length (List.hd char_ls)) in
  parse_map char_ls coords_tbl;
  let coords = Hash.keys coords_tbl |> Batteries.List.of_enum in
  let coords = List.sort sort_coords coords in
  let ants =
    char_ls |> List.flatten |> Batteries.List.unique |> List.filter (fun c -> c <> '.')
  in
  let ant_tbl = Hash.create (List.length ants) in
  add_ants coords_tbl ant_tbl coords;
  print_newline ();
  let ants = Hash.to_list ant_tbl |> List.map snd in
  let antinodes = List.map create_antinode ants |> List.flatten in
  List.iter (add_antinode coords_tbl) antinodes;
  Hash.fold
    (fun _ v a ->
       match v with
       | Antinode | Mix _ -> a + 1
       | _ -> a)
    coords_tbl
    0
;;

let part_b fname =
  let raw_inputs = read_file fname in
  let char_ls = List.map Batteries.String.explode raw_inputs in
  let coords_tbl = Hash.create (List.length char_ls * List.length (List.hd char_ls)) in
  parse_map char_ls coords_tbl;
  let coords = Hash.keys coords_tbl |> Batteries.List.of_enum in
  let coords = List.sort sort_coords coords in
  let max_x = List.map fst coords |> List.fold_left Int.max 0 in
  let max_y = List.map snd coords |> List.fold_left Int.max 0 in
  let ants =
    char_ls |> List.flatten |> Batteries.List.unique |> List.filter (fun c -> c <> '.')
  in
  let ant_tbl = Hash.create (List.length ants) in
  add_ants coords_tbl ant_tbl coords;
  let ants = Hash.to_list ant_tbl |> List.map snd in
  let antinodes = List.map (create_antinode_v2 max_x max_y) ants |> List.flatten in
  List.iter (add_antinode coords_tbl) antinodes;
  Hash.fold
    (fun _ v a ->
       match v with
       | Antinode | Mix _ -> a + 1
       | _ -> a)
    coords_tbl
    0
;;
