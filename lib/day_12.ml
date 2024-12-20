type min_max =
  { min_x : int
  ; max_x : int
  ; min_y : int
  ; max_y : int
  }

type plot =
  { loc : int * int
  ; v : char
  ; borders : int
  ; grouped : bool
  }

type checked =
  { n : bool
  ; s : bool
  ; e : bool
  ; w : bool
  }

type direction =
  | Up
  | Down
  | Left
  | Right

type plot_v2 =
  { loc : int * int
  ; v : char
  ; grouped : bool
  ; dir_walking : direction
  ; dir_checking : direction
  ; borders : int
  ; checked : checked
  }

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

let initial_mapping_v2 arr =
  let max_x = Array.length arr.(0) in
  let max_y = Array.length arr in
  let new_arr =
    Array.init max_y (fun y ->
      Array.init max_x (fun x ->
        { loc = x, y
        ; v = arr.(y).(x)
        ; grouped = false
        ; dir_walking = Up
        ; dir_checking = Up
        ; borders = 0
        ; checked = { n = false; s = false; e = false; w = false }
        }))
  in
  new_arr
;;

let group_plants (arr : plot array array) =
  let max_y = Array.length arr in
  let max_x = Array.length arr.(0) in
  let rec get_group (acc : plot list) (x, y) =
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

let group_plants_v2 (arr : plot_v2 array array) =
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

let min_max ls_ps =
  List.fold_left
    (fun acc p ->
       let min_x =
         if acc.min_x = -1
         then fst p.loc
         else if fst p.loc < acc.min_x
         then fst p.loc
         else acc.min_x
       in
       let max_x = if fst p.loc > acc.max_x then fst p.loc else acc.max_x in
       let min_y =
         if acc.min_y = -1
         then snd p.loc
         else if snd p.loc < acc.min_y
         then snd p.loc
         else acc.min_y
       in
       let max_y = if snd p.loc > acc.max_y then snd p.loc else acc.max_y in
       { min_x; max_x; min_y; max_y })
    { min_x = -1; max_x = 0; min_y = -1; max_y = 0 }
    ls_ps
;;

let count_borders_v2 arr =
  Array.fold_left
    (fun ay y ->
       ay
       + Array.fold_left
           (fun ax x ->
              match x with
              | None -> ax
              | Some v -> ax + v.borders)
           0
           y)
    0
    arr
;;

let count_areas_v2 arr =
  Array.fold_left
    (fun ay y ->
       ay
       + Array.fold_left
           (fun ax x ->
              match x with
              | None -> ax
              | Some _ -> ax + 1)
           0
           y)
    0
    arr
;;

let print_plot_v2 arr =
  Array.iter
    (fun y ->
       Array.iter
         (fun x ->
            print_string
              (match x with
               | None -> "."
               | Some v -> string_of_int v.borders))
         y;
       print_newline ())
    arr;
  print_endline ""
;;

let walk_plot ls_ps =
  let mm = min_max ls_ps in
  let arr =
    Batteries.Array.init_matrix
      (mm.max_y + 1 - mm.min_y)
      (mm.max_x + 1 - mm.min_x)
      (fun y x ->
         Batteries.List.find_opt
           (fun p -> fst p.loc = x + mm.min_x && snd p.loc = y + mm.min_y)
           ls_ps)
  in
  let is_border (x, y) =
    try
      match arr.(y).(x) with
      | None -> true
      | _ -> false
    with
    | _ -> true
  in
  let rec mass_update (x, y) border_dir walk_dir =
    try
      match arr.(y).(x) with
      | None -> ()
      | _ when is_border (move (x, y) border_dir) = false -> ()
      | Some v ->
        (match border_dir with
         | Up ->
           arr.(y).(x) <- Some { v with checked = { v.checked with n = true } };
           mass_update (move (x, y) walk_dir) border_dir walk_dir
         | Right ->
           arr.(y).(x) <- Some { v with checked = { v.checked with e = true } };
           mass_update (move (x, y) walk_dir) border_dir walk_dir
         | Down ->
           arr.(y).(x) <- Some { v with checked = { v.checked with s = true } };
           mass_update (move (x, y) walk_dir) border_dir walk_dir
         | Left ->
           arr.(y).(x) <- Some { v with checked = { v.checked with w = true } };
           mass_update (move (x, y) walk_dir) border_dir walk_dir)
    with
    | _ -> ()
  in
  let check (x, y) dir =
    try
      match dir with
      | Up ->
        (match arr.(y).(x) with
         | Some p ->
           if p.checked.n = true
           then ()
           else if is_border (move (x, y) Up)
           then (
             mass_update (x, y) Up Right;
             mass_update (x, y) Up Left;
             let b = p.borders in
             arr.(y).(x) <- Some { p with borders = b + 1 })
           else ()
         | None -> ())
      | Right ->
        (match arr.(y).(x) with
         | Some p ->
           if p.checked.e = true
           then ()
           else if is_border (move (x, y) Right)
           then (
             mass_update (x, y) Right Up;
             mass_update (x, y) Right Down;
             let b = p.borders in
             arr.(y).(x) <- Some { p with borders = b + 1 })
           else ()
         | None -> ())
      | Down ->
        (match arr.(y).(x) with
         | Some p ->
           if p.checked.s = true
           then ()
           else if is_border (move (x, y) Down)
           then (
             mass_update (x, y) Down Right;
             mass_update (x, y) Down Left;
             let b = p.borders in
             arr.(y).(x) <- Some { p with borders = b + 1 })
           else ()
         | None -> ())
      | Left ->
        (match arr.(y).(x) with
         | Some p ->
           if p.checked.w = true
           then ()
           else if is_border (move (x, y) Left)
           then (
             mass_update (x, y) Left Up;
             mass_update (x, y) Left Down;
             let b = p.borders in
             arr.(y).(x) <- Some { p with borders = b + 1 })
           else ()
         | None -> ())
    with
    | _ -> ()
  in
  let rec aux x y =
    if y = Array.length arr
    then ()
    else if x = Array.length arr.(y)
    then aux 0 (y + 1)
    else (
      check (x, y) Up;
      check (x, y) Right;
      check (x, y) Down;
      check (x, y) Left;
      aux (x + 1) y)
  in
  aux 0 0;
  count_areas_v2 arr, count_borders_v2 arr
;;

let sum_group ls =
  List.length ls * List.fold_left (fun a (p : plot) -> a + 4 - p.borders) 0 ls
;;

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
  let arr = initial_mapping_v2 inputs in
  let grouped = group_plants_v2 arr |> List.map walk_plot in
  List.fold_left (fun acc (area, sides) -> acc + (area * sides)) 0 grouped
;;
