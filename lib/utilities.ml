let read_file fname = Batteries.File.lines_of fname |> Batteries.List.of_enum
let sum_int_list ls = List.fold_left ( + ) 0 ls

let print_type_arr_arr arr inner_printf_str =
  print_endline "[|";
  Array.iter
    (fun y ->
       print_string "[|";
       Array.iter (fun x -> Printf.printf inner_printf_str x) y;
       print_endline "|]")
    arr;
  print_endline "|]"
;;

let get_arr_val arr loc = arr.(snd loc).(fst loc)
let get_next_loc loc dir = fst loc + fst dir, snd loc + snd dir
