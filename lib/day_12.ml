type plot =
  { loc : int * int
  ; v : char
  ; borders : int
  ; grouped : bool
  }

type direction =
  | Up
  | Down
  | Left
  | Right

let add_tuples l r = fst l + fst r, snd l + snd r
let get_value arr (x, y) = arr.(y).(x)

let move loc = function
  | Up -> add_tuples loc (0, -1)
  | Down -> add_tuples loc (0, 1)
  | Left -> add_tuples loc (-1, 0)
  | Right -> add_tuples loc (1, 0)
;;

let count_borders (x, y) arr =
  let gv = get_value arr in
  let v = gv (x, y) in
  let mv dir =
    try if gv (move (x, y) dir) = v then 1 else 0 with
    | _ -> 0
  in
  mv Up + mv Down + mv Left + mv Right
;;

let initial_mapping arr =
  let max_x = Array.length arr.(0) in
  let max_y = Array.length arr in
  let new_arr =
    Array.init max_y (fun y ->
      Array.init max_x (fun x ->
        { loc = x, y
        ; v = arr.(y).(x)
        ; borders = count_borders (x, y) arr
        ; grouped = false
        }))
  in
  new_arr
;;

let group_plants arr =
  let max_y = Array.length arr in
  let max_x = Array.length arr.(0) in
  let rec get_group acc (x, y) =
    let p = get_value arr (x, y) in
    if p.grouped = true
    then acc
    else (
      let mv = move (x, y) in
      let is_valid dir =
        let nx, ny = mv dir in
        if
          p.grouped = false
          && nx >= 0
          && nx < max_x
          && ny >= 0
          && ny < max_y
          && arr.(y).(x).v = arr.(ny).(nx).v
        then true
        else false
      in
      arr.(y).(x) <- { (arr.(y).(x)) with grouped = true };
      let update_acc dir new_acc =
        if is_valid dir then get_group new_acc (mv dir) else new_acc
      in
      let new_acc =
        arr.(y).(x) :: acc
        |> update_acc Up
        |> update_acc Down
        |> update_acc Left
        |> update_acc Right
      in
      new_acc)
  in
  let rec aux x y acc =
    if y = max_y
    then acc
    else if x = max_x
    then aux 0 (y + 1) acc
    else if arr.(y).(x).grouped = true
    then aux (x + 1) y acc
    else aux (x + 1) y (get_group [] (x, y) :: acc)
  in
  aux 0 0 []
;;

let sum_group ls = List.length ls * List.fold_left (fun a p -> a + 4 - p.borders) 0 ls
let print_plot p = Printf.printf "(%c, %d) " p.v p.borders
let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum

let part_a fname =
  let raw_inputs = read_file fname in
  let inputs =
    List.map Batteries.String.explode raw_inputs
    |> List.map Batteries.Array.of_list
    |> Batteries.Array.of_list
  in
  let arr = initial_mapping inputs in
  let grouped = group_plants arr in
  List.map sum_group grouped |> List.fold_left ( + ) 0
;;

(* target is 236 *)
let part_b fname =
  let raw_inputs = read_file fname in
  let inputs =
    List.map Batteries.String.explode raw_inputs
    |> List.map Batteries.Array.of_list
    |> Batteries.Array.of_list
  in
  let arr = initial_mapping inputs in
  let grouped = group_plants arr in
  List.iter
    (fun l ->
       Printf.printf
         "Nodes: %d; Edges: %d; Touching: %d ::: "
         (List.length l)
         (List.length l * 4)
         (List.fold_left (fun a p -> p.borders + a) 0 l);
         List.iter
         (fun p -> Printf.printf "(%c, %d); " p.v p.borders)
         l;
       print_newline ())
    grouped;
  List.map sum_group grouped |> List.fold_left ( + ) 0
;;
