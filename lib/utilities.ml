let read_file fname =
  Batteries.File.lines_of fname |> Batteries.List.of_enum
;;
