module Hash = struct
  include Batteries.Hashtbl
end

let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum
let print_guide g = Printf.printf "(%d, %d)\n" (fst g) (snd g)

let print_int_ls ls =
  print_string "[";
  List.iter (Printf.printf "%d, ") ls;
  print_endline "]"
;;

let parse_guide g =
  let ls = String.split_on_char '|' g in
  assert (List.length ls = 2);
  int_of_string @@ List.nth ls 0, int_of_string @@ List.nth ls 1
;;

let split_list key ls =
  let rec aux acc switch = function
    | [] -> List.rev (fst acc), List.rev (snd acc)
    | hd :: tl when hd = key -> aux acc true tl
    | hd :: tl when switch = false -> aux (hd :: fst acc, snd acc) false tl
    | hd :: tl -> aux (fst acc, hd :: snd acc) true tl
  in
  aux ([], []) false ls
;;

let get_mid_ls ls =
  let mid = List.length ls / 2 in
  List.nth ls mid
;;

let is_valid_instruction g_tbl ls =
  let rec aux = function
    | [] -> true
    | [ _ ] -> true
    | hd :: (md :: tl as ls) -> Hash.mem g_tbl (hd, md) && aux (hd :: tl) && aux ls
  in
  aux ls
;;

let sort_instructions g_tbl left right =
  if Hash.mem g_tbl (left, right)
  then 1
  else if Hash.mem g_tbl (right, left)
  then -1
  else 0
;;

let part_a fname =
  let raw_guide, raw_inputs = read_file fname |> split_list "" in
  let guides = List.map parse_guide raw_guide in
  let g_tbl = Hash.create (List.length guides) in
  List.iter (fun x -> Hash.add g_tbl x true) guides;
  let inputs =
    List.map (String.split_on_char ',') raw_inputs |> List.map (List.map int_of_string)
  in
  Printf.printf "\ninput length: %d\n" (List.length inputs);
  let output = List.filter (is_valid_instruction g_tbl) inputs in
  Printf.printf "output length: %d\n\n" (List.length output);
  List.map get_mid_ls output |> List.fold_left ( + ) 0
;;

let part_b fname =
  let raw_guide, raw_inputs = read_file fname |> split_list "" in
  let guides = List.map parse_guide raw_guide in
  let g_tbl = Hash.create (List.length guides) in
  List.iter (fun x -> Hash.add g_tbl x true) guides;
  let inputs =
    List.map (String.split_on_char ',') raw_inputs |> List.map (List.map int_of_string)
  in
  Printf.printf "\ninput length: %d\n" (List.length inputs);
  let unsorted = List.filter (fun x -> not (is_valid_instruction g_tbl x)) inputs in
  let output = List.map (List.sort (sort_instructions g_tbl)) unsorted in
  Printf.printf "output length: %d\n\n" (List.length output);
  List.map get_mid_ls output |> List.fold_left ( + ) 0
;;
