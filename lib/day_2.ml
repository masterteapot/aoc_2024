let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum

let split_lines ~char txt_lines =
  List.map (fun s -> String.split_on_char char s) txt_lines
;;

let find_matches left right =
  let rec aux counter x = function
    | [] -> x, counter
    | hd :: tl when hd = x -> aux (counter + 1) x tl
    | hd :: tl when hd < x -> aux counter x tl
    | _ -> x, counter
  in
  List.map (fun x -> aux 0 x right) left
;;

let transform ls =
    let rec aux acc = function
        | [] -> List.rev acc
        | hd :: tl -> aux (hd :: acc) tl
    in
    aux [] ls
;;

let print ls =
    List.iter print_endline ls
;;

(* let part_a fname = *)
let fname = "data/examples/day_2a.txt" in
let txt_lines = read_file fname in
let split = split_lines ~char:' ' txt_lines in
let trans = transform split in
List.iter print trans;

(* let part_b fname =  *)
(*     let txt_lines = read_file fname in *)
(*     let split = split_lines txt_lines in *)
(*     let nums = get_int_lists split in *)
(*     let left = fst nums |> List.sort compare in *)
(*     let right = snd nums |> List.sort compare in *)
(*     let matches = find_matches left right in *)
(*     List.fold_left (fun acc (l, r) -> acc + (l * r) ) 0 matches *)
(* ;; *)
