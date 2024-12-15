let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum

type button_name =
  | A
  | B
  | Prize

type button =
  { name : button_name
  ; x : int
  ; y : int
  ; cost : int
  }

let evaluate (ba, bb, bp) =
  let a_perf = (ba.x + ba.y) / ba.cost in
  let b_perf = (bb.x + bb.y) / bb.cost in
  let rec aux first second num_first num_second =
    if num_second > 100
    then None
    else if
      (num_first * first.x) + (num_second * second.x) = bp.x
      && (num_first * first.y) + (num_second * second.y) = bp.y
    then Some (num_first, num_second)
    else if num_first > 100
    then aux first second 0 (num_second + 1)
    else aux first second (num_first + 1) num_second
  in
  if a_perf > b_perf
  then aux ba bb 1 0
  else (
    match aux bb ba 1 0 with
    | None -> None
    | Some (b_res, a_res) -> Some (a_res, b_res))
;;

let evaluate_v2 (ba, bb, bp) =
  let a_perf = (ba.x + ba.y) / ba.cost in
  let b_perf = (bb.x + bb.y) / bb.cost in
  let rec aux first second num_first num_second =
    let max_first = Int.min (bp.x / first.x) (bp.y / first.y) in
    if max_first - num_first > (first.x * second.x) + (first.y * second.y)
    then None
    else if num_first < 0
    then None
    else if
      (num_first * first.x) + (num_second * second.x) = bp.x
      && (num_first * first.y) + (num_second * second.y) = bp.y
    then Some (num_first, num_second)
    else if
      (num_first * first.x) + (num_second * second.x) > bp.x
      || (num_first * first.y) + (num_second * second.y) > bp.y
    then aux first second (num_first - 1) 0
    else aux first second num_first (num_second + 1)
  in
  if a_perf > b_perf
  then (
    let max_first = Int.min (bp.x / ba.x) (bp.y / ba.y) in
    aux ba bb max_first 0)
  else (
    let max_first = Int.min (bp.x / bb.x) (bp.y / bb.y) in
    match aux bb ba max_first 0 with
    | None -> None
    | Some (b_res, a_res) -> Some (a_res, b_res))
;;

let split_machines ls =
  let get_val s sub =
    let rec gv_aux index acc =
      try
        match s.[index] with
        | '0' .. '9' as v -> gv_aux (index + 1) (v :: acc)
        | _ -> int_of_string @@ Batteries.String.of_list (List.rev acc)
      with
      | _ -> int_of_string @@ Batteries.String.of_list (List.rev acc)
    in
    let index = Batteries.String.find s sub in
    gv_aux (index + String.length sub) []
  in
  let rec aux inner outer = function
    | [] -> List.rev ((List.nth inner 2, List.nth inner 1, List.nth inner 0) :: outer)
    | "" :: tl ->
      assert (List.length inner = 3);
      aux [] ((List.nth inner 2, List.nth inner 1, List.nth inner 0) :: outer) tl
    | hd :: tl when Batteries.String.starts_with hd "Button A:" ->
      aux
        ({ name = A; x = get_val hd "X+"; y = get_val hd "Y+"; cost = 3 } :: inner)
        outer
        tl
    | hd :: tl when Batteries.String.starts_with hd "Button B:" ->
      aux
        ({ name = B; x = get_val hd "X+"; y = get_val hd "Y+"; cost = 1 } :: inner)
        outer
        tl
    | hd :: tl when Batteries.String.starts_with hd "Prize:" ->
      aux
        ({ name = Prize; x = get_val hd "X="; y = get_val hd "Y="; cost = 0 } :: inner)
        outer
        tl
    | x :: _ ->
      print_endline x;
      failwith "shouldn't be here"
  in
  aux [] [] ls
;;

let split_machines_v2 ls =
  let get_val s sub =
    let rec gv_aux index acc =
      try
        match s.[index] with
        | '0' .. '9' as v -> gv_aux (index + 1) (v :: acc)
        | _ -> int_of_string @@ Batteries.String.of_list (List.rev acc)
      with
      | _ -> int_of_string @@ Batteries.String.of_list (List.rev acc)
    in
    let index = Batteries.String.find s sub in
    gv_aux (index + String.length sub) []
  in
  let rec aux inner outer = function
    | [] -> List.rev ((List.nth inner 2, List.nth inner 1, List.nth inner 0) :: outer)
    | "" :: tl ->
      assert (List.length inner = 3);
      aux [] ((List.nth inner 2, List.nth inner 1, List.nth inner 0) :: outer) tl
    | hd :: tl when Batteries.String.starts_with hd "Button A:" ->
      aux
        ({ name = A; x = get_val hd "X+"; y = get_val hd "Y+"; cost = 3 } :: inner)
        outer
        tl
    | hd :: tl when Batteries.String.starts_with hd "Button B:" ->
      aux
        ({ name = B; x = get_val hd "X+"; y = get_val hd "Y+"; cost = 1 } :: inner)
        outer
        tl
    | hd :: tl when Batteries.String.starts_with hd "Prize:" ->
      aux
        ({ name = Prize
         ; x = get_val hd "X=" + 10000000000000
         ; y = get_val hd "Y=" + 10000000000000
         ; cost = 0
         }
         :: inner)
        outer
        tl
    | x :: _ ->
      print_endline x;
      failwith "shouldn't be here"
  in
  aux [] [] ls
;;

let print_results = function
  | None -> print_endline "none"
  | Some (x, y) -> Printf.printf "\nPress A: %d; Press B: %d\n\n" x y
;;

let part_a fname =
  let raw_inputs = read_file fname in
  let inputs = split_machines raw_inputs in
  let results = List.map evaluate_v2 inputs in
  (*List.iter print_results results;*)
  List.fold_left
    (fun acc x ->
       match x with
       | None -> acc
       | Some (a, b) -> (a * 3) + (b * 1) + acc)
    0
    results
;;

let part_b fname =
  let raw_inputs = read_file fname in
  let inputs = split_machines_v2 raw_inputs in
  let results = List.map evaluate_v2 inputs in
  (*List.iter print_results results;*)
  List.fold_left
    (fun acc x ->
       match x with
       | None -> acc
       | Some (a, b) -> (a * 3) + (b * 1) + acc)
    0
    results
;;
