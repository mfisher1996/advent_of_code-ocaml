
let () = print_endline "Day 1 Part 1: "

let () = 
    let file = "./prod_1"
    in
    let input = open_in file in
    let rec read_file inner outer input_chan = match input_line input_chan with
        | exception End_of_file -> outer
        | "" -> read_file [] (inner::outer) input
        | line -> read_file (line::inner) outer input_chan
    in
    let rec aux acc = function
        | [] -> acc
        | line::rest -> aux (int_of_string line + acc) rest
    in
    let rec to_ints acc = function
        | [] -> acc
        | line::rest -> to_ints (aux 0 line :: acc) rest
    in
    let lines = read_file [] [] input in
    let ints = to_ints [] lines in
    print_int (List.fold_left (fun max x -> if x > max then x else max) 0 ints)

let () = print_newline ()
    
let () = print_endline "Day 1 Part 2: "

let () = 
    let file = "./prod_1"
    in
    let input = open_in file in
    let rec read_file inner outer input_chan = match input_line input_chan with
        | exception End_of_file -> outer
        | "" -> read_file [] (inner::outer) input
        | line -> read_file (line::inner) outer input_chan
    in
    let rec aux acc = function
        | [] -> acc
        | line::rest -> aux (int_of_string line + acc) rest
    in
    let rec to_ints acc = function
        | [] -> acc
        | line::rest -> to_ints (aux 0 line :: acc) rest
    in
    let lines = read_file [] [] input in
    let ints = to_ints [] lines in
    let sorted = List.sort (fun a b -> if (b - a) > 0 then 1 else -1 ) ints
    in
    let top_three = function
    | x::y::z::_ -> x + y + z
    | _ -> 0
    in
    print_int (top_three sorted)






