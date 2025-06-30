let rec read_input (input : in_channel) =
  match input_line input with
  | exception End_of_file -> []
  | line -> line :: read_input input

let mult = function
  | h when String.starts_with ~prefix:"ul(" h ->
      let mults =
        String.to_seq h |> Seq.drop 3
        |> Seq.take_while (fun c -> c != ')')
        |> String.of_seq
      in
      if
        String.to_seq mults
        |> Seq.for_all (fun s ->
               match s with '0' .. '9' | ',' -> true | _ -> false)
      then
        let nums = String.split_on_char ',' mults in
        List.map int_of_string nums |> List.fold_left ( * ) 1
      else 0
  | _ -> 0

let do_nt = function
  | h when String.starts_with ~prefix:"o()" h ->
      String.split_on_char 'm' h |> List.map mult |> List.fold_left ( + ) 0
  | h when String.starts_with ~prefix:"on't()" h -> 0
  | h -> String.split_on_char 'm' h |> List.map mult |> List.fold_left ( + ) 0

let part_1 (input : in_channel) =
  let input =
    read_input input |> List.fold_left ( ^ ) "" |> String.split_on_char 'm'
  in
  print_string "\tPart 1: ";
  print_int @@ List.fold_left ( + ) 0 @@ List.map mult input;
  print_newline ()

let part_2 (input : in_channel) =
  let input =
    read_input input |> List.fold_left ( ^ ) "" |> String.split_on_char 'd'
  in
  print_string "\tPart 2: ";
  print_int @@ List.fold_left ( + ) 0 @@ List.map do_nt input;
  print_newline ()
