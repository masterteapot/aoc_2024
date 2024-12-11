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

let rec print_vals = function
  | 0, _ -> ()
  | i, None ->
    print_string "., ";
    print_vals (i - 1, None)
  | i, Some x ->
    Printf.printf "%d, " x;
    print_vals (i - 1, Some x)
;;

let nums_to_move num_empty arr =
  let arr_len = Array.length arr in
  let rec aux acc index =
    if List.length acc = num_empty
    then List.rev acc, arr
    else (
      match arr.(index) with
      | None -> aux acc (index - 1)
      | Some x ->
        arr.(index) <- None;
        aux (x :: acc) (index - 1))
  in
  aux [] (arr_len - 1)
;;

(* Could also batch finding all nones to make fewer search queries *)
let rec move_nums num_ls arr =
  match num_ls with
  | [] -> arr
  | hd :: tl ->
    let i = Batteries.Array.findi (fun x -> x = None) arr in
    arr.(i) <- Some hd;
    move_nums tl arr
;;


(* Need to handle the cases when the moved group is smaller than the blanks, need to insert new blank group *)
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
  let rec aux = function
    | [] -> arr
    | i :: tl ->
      let min_size, _ = arr.(i) in
      (match find_lower_match min_size 0 i with
       | None -> aux tl
       | Some new_i ->
         arr.(new_i) <- arr.(i);
         arr.(i) <- min_size, None;
         Array.iter print_vals arr;
         print_newline ();
         aux tl)
  in
  aux indices
;;

let count_none_below_some num_some arr =
  let rec aux counter num_none ls =
    if counter = num_some
    then num_none
    else (
      match ls with
      | [] -> failwith "something went wrong, list shouldn't be longer than the num none"
      | None :: tl -> aux (counter + 1) (num_none + 1) tl
      | _ :: tl -> aux (counter + 1) num_none tl)
  in
  aux 0 0 (Batteries.Array.to_list arr)
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
  let arr_len = Array.length arr in
  let num_none = Array.fold_left (fun a x -> if x = None then a + 1 else a) 0 arr in
  let num_some =
    Array.fold_left
      (fun a x ->
         match x with
         | Some _ -> a + 1
         | _ -> a)
      0
      arr
  in
  assert (num_some = arr_len - num_none);
  let num_none_below_some = count_none_below_some num_some arr in
  let to_move, arr = nums_to_move num_none_below_some arr in
  let arr = move_nums to_move arr in
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
  (* let arr_len = Array.length arr in *)
  let arr = move_nums_v2 arr in
  let checksum = ref 0 in
  Array.iteri
    (fun i (num, x) ->
       match x with
       | None -> ()
       | Some v -> checksum := (i * v * num) + !checksum)
    arr;
  !checksum
;;
