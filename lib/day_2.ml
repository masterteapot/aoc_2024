type direction =
  | Increasing
  | Decreasing
  | Unknown

let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum

let split_lines ~char txt_lines =
  List.map (fun s -> String.split_on_char char s) txt_lines
;;

let transform ls =
  let rec aux acc dir = function
    | [] -> acc
    | [ _ ] -> acc
    | hd :: md :: tl ->
      let new_dir =
        if md > hd then Increasing else if md < hd then Decreasing else Unknown
      in
      let diff = Int.abs @@ (hd - md) in
      let new_acc = acc && diff <= 3 && diff > 0 && (new_dir = dir || dir = Unknown) in
      aux new_acc (if dir = Unknown then new_dir else dir) (md :: tl)
  in
  aux true Unknown ls
;;

let rec make_range bottom top acc =
  if bottom >= top then acc else make_range bottom (top - 1) ((top - 1) :: acc)
;;

let print ls = List.iter print_endline ls

let part_a fname =
  let txt_lines = read_file fname in
  let split =
    split_lines ~char:' ' txt_lines |> List.map (fun x -> List.map int_of_string x)
  in
  let trans = List.map transform split in
  List.fold_left (fun acc x -> if x = true then acc + 1 else acc) 0 trans
;;

let part_b fname =
  let txt_lines = read_file fname in
  let split =
    split_lines ~char:' ' txt_lines |> List.map (fun x -> List.map int_of_string x)
  in
  let trans =
    List.map
      (fun ls ->
        let nums = make_range 0 (List.length ls) [] in
        List.fold_left
          (fun acc n -> acc || transform (Batteries.List.remove_at n ls))
          false
          nums)
      split
  in
  List.fold_left (fun acc x -> if x = true then acc + 1 else acc) 0 trans
;;
