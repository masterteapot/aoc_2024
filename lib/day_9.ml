type entry =
  { old_index : int
  ; new_index : int
  ; size : int
  ; value : int option
  }

type direction =
  | Up
  | Down

let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum

let parse_part_a ls =
  let rec aux acc empty counter = function
    | [] -> List.rev acc
    | hd :: tl when empty = true ->
      let new_ls = Batteries.List.init hd (fun _ -> None) in
      aux (new_ls :: acc) false (counter + 1) tl
    | hd :: tl ->
      let new_ls = Batteries.List.init hd (fun _ -> Some (counter / 2)) in
      aux (new_ls :: acc) true (counter + 1) tl
  in
  aux [] false 0 ls
;;

let parse_part_b ls =
  let rec aux acc empty counter = function
    | [] -> List.rev acc
    | hd :: tl when empty = true -> aux ((hd, None) :: acc) false (counter + 1) tl
    | hd :: tl -> aux ((hd, Some (counter / 2)) :: acc) true (counter + 1) tl
  in
  aux [] false 0 ls
;;

let move_nums arr =
  let rec aux li hi direction =
    if li >= hi
    then ()
    else (
      match direction with
      | Down ->
        (match arr.(hi) with
         | None -> aux li (hi - 1) Down
         | Some _ -> aux li hi Up)
      | Up ->
        (match arr.(li) with
         | None ->
           arr.(li) <- arr.(hi);
           arr.(hi) <- None;
           aux (li + 1) (hi - 1) Down
         | Some _ -> aux (li + 1) hi Up))
  in
  aux 0 (Array.length arr - 1) Down
;;

let move_nums_v2 arr =
  let indices =
    Batteries.Array.fold_lefti
      (fun acc i x ->
         match x with
         | _, None -> acc
         | _, Some _ -> i :: acc)
      []
      arr
  in
  let rec find_lower_match min_size new_index old_index =
    if new_index = old_index
    then None
    else (
      let new_size, new_val = arr.(new_index) in
      if new_val = None && new_size >= min_size
      then Some new_index
      else find_lower_match min_size (new_index + 1) old_index)
  in
  let rec aux acc = function
    | [] -> acc
    | i :: tl ->
      let size, v = arr.(i) in
      (match find_lower_match size 0 i with
       | None -> aux acc tl
       | Some new_i ->
         let entry = { old_index = i; new_index = new_i; size; value = v } in
         arr.(new_i) <- fst arr.(new_i) - fst arr.(i), None;
         arr.(i) <- size, None;
         aux (entry :: acc) tl)
  in
  let rec rebuild i acc mtbl =
    if i = Array.length arr
    then List.rev acc
    else (
      match arr.(i) with
      | n, Some v -> rebuild (i + 1) ((n, Some v) :: acc) mtbl
      | n, None ->
        let moved =
          Batteries.Hashtbl.find_all mtbl i
          |> List.sort (fun l r ->
            if l.old_index > r.old_index
            then 1
            else if l.old_index = r.old_index
            then 0
            else -1)
          |> List.map (fun x -> x.size, x.value)
        in
        rebuild (i + 1) ((n, None) :: (moved @ acc)) mtbl)
  in
  let mtbl =
    aux [] indices |> List.map (fun x -> x.new_index, x) |> Batteries.Hashtbl.of_list
  in
  rebuild 0 [] mtbl
;;

let calculate_p2 ls =
  let rec aux counter acc = function
    | [] -> acc
    | (d, None) :: tl -> aux (counter + d) acc tl
    | (0, Some _) :: tl -> aux counter acc tl
    | (d, Some v) :: tl -> aux (counter + 1) (acc + (counter * v)) ((d - 1, Some v) :: tl)
  in
  aux 0 0 ls
;;

let part_a fname =
  let raw_inputs = read_file fname in
  let ls =
    List.map Batteries.String.explode raw_inputs
    |> List.flatten
    |> List.map Batteries.String.of_char
    |> List.map int_of_string
  in
  let arr = ls |> parse_part_a |> List.flatten |> Batteries.Array.of_list in
  move_nums arr;
  let checksum = ref 0 in
  Array.iteri
    (fun i x ->
       match x with
       | None -> ()
       | Some v -> checksum := (i * v) + !checksum)
    arr;
  !checksum
;;

let part_b fname =
  let raw_inputs = read_file fname in
  let ls =
    List.map Batteries.String.explode raw_inputs
    |> List.flatten
    |> List.map Batteries.String.of_char
    |> List.map int_of_string
  in
  let arr = ls |> parse_part_b |> Batteries.Array.of_list in
  let ls = move_nums_v2 arr in
  calculate_p2 ls
;;
