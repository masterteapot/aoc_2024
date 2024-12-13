module Hash = struct
  include Batteries.Hashtbl
end

let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum

let rec evaluate rounds ls =
  let rec aux acc = function
    | [] -> acc
    | 0 :: tl -> aux (1 :: acc) tl
    | n :: tl when n >= 10 && n < 100 -> aux ((n / 10) :: Int.rem n 10 :: acc) tl
    | n :: tl when n >= 1_000 && n < 10_000 -> aux ((n / 100) :: Int.rem n 100 :: acc) tl
    | n :: tl when n >= 100_000 && n < 1_000_000 ->
      aux ((n / 1_000) :: Int.rem n 1_000 :: acc) tl
    | n :: tl when n >= 10_000_000 && n < 100_000_000 ->
      aux ((n / 10_000) :: Int.rem n 10_000 :: acc) tl
    | n :: tl when n >= 1_000_000_000 && n < 10_000_000_000 ->
      aux ((n / 100_000) :: Int.rem n 100_000 :: acc) tl
    | n :: tl when n >= 100_000_000_000 && n < 1_000_000_000_000 ->
      aux ((n / 1_000_000) :: Int.rem n 1_000_000 :: acc) tl
    | n :: tl when n >= 10_000_000_000_000 && n < 100_000_000_000_000 ->
      aux ((n / 10_000_000) :: Int.rem n 10_000_000 :: acc) tl
    | n :: _ when n >= 100_000_000_000_000 -> failwith "unexpectedly high number"
    | n :: tl -> aux ((n * 2024) :: acc) tl
  in
  assert (rounds >= 0);
  if rounds = 0 then ls else evaluate (rounds - 1) (aux [] ls)
;;

let rec evaluate_v2 tbl rounds ls =
  let rec aux acc = function
    | [] -> acc
    | (x, 0) :: tl -> aux ((x, 1) :: acc) tl
    | (x, n) :: tl when n >= 10 && n < 100 -> splitter x n 10 acc tl
    | (x, n) :: tl when n >= 1_000 && n < 10_000 -> splitter x n 100 acc tl
    | (x, n) :: tl when n >= 100_000 && n < 1_000_000 -> splitter x n 1_000 acc tl
    | (x, n) :: tl when n >= 10_000_000 && n < 100_000_000 -> splitter x n 10_000 acc tl
    | (x, n) :: tl when n >= 1_000_000_000 && n < 10_000_000_000 ->
      splitter x n 100_000 acc tl
    | (x, n) :: tl when n >= 100_000_000_000 && n < 1_000_000_000_000 ->
      splitter x n 1_000_000 acc tl
    | (x, n) :: tl when n >= 10_000_000_000_000 && n < 100_000_000_000_000 ->
      splitter x n 10_000_000 acc tl
    | (_, n) :: _ when n >= 100_000_000_000_000 -> failwith "unexpectedly high number"
    | (x, n) :: tl -> aux ((x, n * 2024) :: acc) tl
  and splitter x n b acc tl = aux ((x, n / b) :: (x, Int.rem n b) :: acc) tl in
  assert (rounds >= 0);
  if rounds = 0
  then ls
  else if Int.rem rounds 5 = 0
  then (
    let dedupped = Hash.create (List.length ls) in
    List.iter
      (fun (x, n) ->
         if Hash.mem dedupped n
         then Hash.replace dedupped n (Hash.find dedupped n + x)
         else Hash.add dedupped n x)
      ls;
    let dd_ls = Hash.fold (fun k v a -> (v, k) :: a) dedupped [] in
    evaluate_v2 tbl (rounds - 1) (aux [] dd_ls))
  else evaluate_v2 tbl (rounds - 1) (aux [] ls)
;;

let part_a fname =
  let raw_inputs = read_file fname in
  let inputs =
    List.fold_left ( ^ ) "" raw_inputs
    |> String.split_on_char ' '
    |> List.map int_of_string
  in
  let output = evaluate 25 inputs in
  List.length output
;;

let part_b fname =
  let raw_inputs = read_file fname in
  let inputs =
    List.fold_left ( ^ ) "" raw_inputs
    |> String.split_on_char ' '
    |> List.map int_of_string
    |> List.map (fun x -> 1, x)
  in
  let tbl = Hash.create 10_000 in
  let output = evaluate_v2 tbl 75 inputs in
  List.fold_left (fun a (k, _) -> k + a) 0 output
;;
