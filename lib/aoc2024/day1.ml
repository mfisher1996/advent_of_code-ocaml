let rec read_input (input : in_channel) (left_acc : string list)
    (right_acc : string list) =
  match input_line input with
  | exception End_of_file ->
      (left_acc |> List.map int_of_string, right_acc |> List.map int_of_string)
  | line ->
      let chars = String.to_seq line in
      let left =
        chars
        |> Seq.take_while (fun c ->
               match c with '0' .. '9' -> true | _ -> false)
        |> String.of_seq
      in
      let right =
        chars |> Seq.drop (String.length left) |> String.of_seq |> String.trim
      in
      read_input input (left :: left_acc) (right :: right_acc)

let part_1 (input : in_channel) =
  let left, right = read_input input [] [] in
  let left_sort, right_sort =
    ( left |> List.fast_sort (fun a b -> a - b),
      right |> List.fast_sort (fun a b -> a - b) )
  in
  print_string "\tPart 1: ";
  print_int
  @@ List.fold_left2
       (fun acc left right -> acc + abs (left - right))
       0 left_sort right_sort;
  print_newline ()

let part_2 (input : in_channel) =
  let left, right = read_input input [] [] in
  let right_sort = right |> List.fast_sort (fun a b -> a - b) in
  print_string "\tPart 2: ";
  print_int
  @@ List.fold_left
       (fun acc num ->
         acc
         + (num * (List.length @@ List.find_all (fun x -> x = num) right_sort)))
       0 left;
  print_newline ()
