let rec read_file sum acc input_chan = match input_line input_chan with
    | exception End_of_file -> acc
    | line -> read_file (sum + int_of_string line) acc input_chan
    
    
let part_1 (input: in_channel) =
    let ints = read_file 0 [] input in
    let sorted = List.sort (fun a b -> if (b - a) > 0 then 1 else -1 ) ints
    in
    let top_three = function
    | x::y::z::_ -> x + y + z
    | _ -> 0
    in
    print_endline "Part 2: ";
    print_int (top_three sorted)

let part_2 (input: in_channel) =
    let ints = read_file 0 [] input in
    let sorted = List.sort (fun a b -> if (b - a) > 0 then 1 else -1 ) ints
    in
    let top_three = function
    | x::y::z::_ -> x + y + z
    | _ -> 0
    in
    print_endline "Part 2: ";
    print_int (top_three sorted)
