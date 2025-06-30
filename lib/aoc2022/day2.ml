open Files

let part_1 (input : in_channel) =
  let get_play c d =
    match (c, d) with
    | a, 'X' -> if a == 0 then 3 else a
    | a, 'Y' -> a + 4
    | a, _ -> ((a + 1) mod 3) + 7
  in
  let rec read_file_2 acc file =
    match input_line file with
    | exception End_of_file -> acc
    | line when String.length line == 3 ->
        read_file_2
          (get_play
             (Files.code_of_char 'A' (String.get line 0))
             (String.get line 2)
          + acc)
          input
    | _ -> read_file_2 acc file
  in
  print_int (read_file_2 0 input);
  print_newline ()

let part_2 (input : in_channel) =
  let get_score c d =
    match (c, d) with
    | a, b when a == (b + 1) mod 3 -> b + 1
    | a, b when (a + 1) mod 3 == b -> b + 7
    | _, b -> b + 4
  in
  let rec read_file_1 acc file =
    match input_line file with
    | exception End_of_file -> acc
    | line when String.length line == 3 ->
        read_file_1
          (get_score
             (code_of_char 'A' (String.get line 0))
             (code_of_char 'X' (String.get line 2))
          + acc)
          input
    | _ -> read_file_1 acc input
  in
  print_int (read_file_1 0 input);
  print_endline "\nPart 2: "
