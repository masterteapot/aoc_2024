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
  Array.iter
    (fun a ->
       Array.iter print_plot a;
       print_endline "")
    arr;
  Array.length arr
;;
