
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
let () = 
    let file = "./prod_2"
    in
    let code_of_char start = function
        | a -> Char.code a - Char.code start
    in
    let get_score c d = match (c, d) with
        | a, b when a == (b + 1) mod  3  -> b + 1
        | a, b when  (a + 1) mod  3  == b -> b + 7
        | _, b -> b + 4
    in 
    let input = open_in file in
    let rec read_file acc file = match input_line file with
        | exception End_of_file -> acc
        | line when String.length line == 3 ->  read_file ( (get_score (code_of_char 'A' (String.get line 0))  (code_of_char 'X' (String.get line 2))) + acc ) input
        | _ -> read_file acc input
    in 
    print_int (read_file 0 input);
    print_newline ()
