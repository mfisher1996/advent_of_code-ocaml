let process (file: string) (f: in_channel -> 'a) : 'a =
    let input = open_in file
    in
    let result = f input
    in
    close_in input;
    result

let code_of_char start = function a -> 
    Char.code a - Char.code start
