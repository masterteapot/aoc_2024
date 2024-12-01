let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum

let get_int_lists ls =
  let rec aux acc1 acc2 = function
    | [] -> acc1, acc2
    | hd :: tl ->
      assert (List.length hd = 2);
      let first = List.nth hd 0 |> int_of_string in
      let second = List.nth hd 1 |> int_of_string in
      aux (first :: acc1) (second :: acc2) tl
  in
  aux [] [] ls
;;

let split_lines txt_lines =
  List.map
    (fun s ->
      let ls = String.split_on_char ' ' s in
      List.filter (fun x -> x <> "") ls)
    txt_lines
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

let part_a fname =
  let txt_lines = read_file fname in
  let split = split_lines txt_lines in
  let nums = get_int_lists split in
  let left = fst nums |> List.sort compare in
  let right = snd nums |> List.sort compare in
  let diffs = List.map2 (fun x y -> Int.abs (x - y)) left right in
  List.fold_left ( + ) 0 diffs
;;

let part_b fname = 
    let txt_lines = read_file fname in
    let split = split_lines txt_lines in
    let nums = get_int_lists split in
    let left = fst nums |> List.sort compare in
    let right = snd nums |> List.sort compare in
    let matches = find_matches left right in
    List.fold_left (fun acc (l, r) -> acc + (l * r) ) 0 matches
;;
