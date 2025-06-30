let rec read_input (input : in_channel) (acc : string list) =
  match input_line input with
  | exception End_of_file -> acc
  | line -> read_input input (line :: acc)

let rec valid (line : int list) (last : int) (pos : bool) (damp : bool) =
  match line with
  | [] -> true
  | h :: t when damp ->
      if h > last = pos && abs (h - last) <= 3 && abs (h - last) > 0 then
        valid t h (h > last) damp
      else valid t last pos false
  | h :: t ->
      if h > last = pos && abs (h - last) <= 3 && abs (h - last) > 0 then
        valid t h (h > last) damp
      else false

let part_1 (input : in_channel) =
  let input =
    read_input input []
    |> List.map (String.split_on_char ' ')
    |> List.map (List.map int_of_string)
  in
  print_string "\tPart 1: ";
  print_int
    (input
    |> List.filter (fun line ->
           valid (List.tl line) (List.hd line)
             (List.nth line 1 > List.hd line)
             false)
    |> List.length);
  print_newline ()

let part_2 (input : in_channel) =
  let input =
    read_input input []
    |> List.map (String.split_on_char ' ')
    |> List.map (List.map int_of_string)
  in
  print_string "\tPart 2: ";
  print_int
    (input
    |> List.filter (fun line ->
           if
             valid (List.tl line) (List.hd line)
               (List.nth line 1 > List.hd line)
               true
           then true
           else
             let line = List.rev line in
             valid (List.tl line) (List.hd line)
               (List.nth line 1 > List.hd line)
               true)
    |> List.length);
  print_newline ()
