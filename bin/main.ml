
let () = print_endline "Day 1 Part 1: "

let () = 
    let file = "./prod_1"
    in
    let input = open_in file in
    let rec read_file sum acc input_chan = match input_line input_chan with
        | exception End_of_file -> acc
        | "" -> read_file 0 (sum::acc) input
        | line -> read_file (sum + int_of_string line) acc input_chan
    in
    let ints = read_file 0 [] input in
    let sorted = List.sort (fun a b -> if (b - a) > 0 then 1 else -1 ) ints
    in
    print_int (List.nth sorted 0);
    print_newline ();
    let top_three = function
    | x::y::z::_ -> x + y + z
    | _ -> 0
    in
    print_endline "Part 2: ";
    print_int (top_three sorted)



let () = print_newline ()
    
let () = print_endline "Day 2 Part 1: "
(*
let () = 
    let file = "./prod_1"
    in
    let input = open_in file in
    let rec read_file acc = match input_line input with
        | exception End_of_file -> acc
        | "" -> acc
        | line -> read_file (
            String.get line 0 , String.get line 2 :: acc
        ) input
    in 
    *)
