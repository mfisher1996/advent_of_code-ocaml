let part_1 (input : in_channel) =
  let get_char_code c =
    match c with
    | a when a >= 'a' && a <= 'z' -> Char.code a - Char.code 'a' + 1
    | a when a >= 'A' && a <= 'Z' -> Char.code a - Char.code 'A' + 27
    | _ -> 0
  in
  let find_common s1 s2 =
    (String.trim
       (String.map
          (fun c ->
            match String.index_opt s1 c with
            | Some x -> String.get s1 x
            | None -> ' ')
          s2)).[0]
  in
  let read_file file =
    let rec read_aux acc file =
      match input_line file with
      | exception End_of_file -> acc
      | line when String.length line > 2 ->
          let half_len = String.length line / 2 in
          read_aux
            (get_char_code
               (find_common
                  (String.sub line 0 half_len)
                  (String.sub line half_len (String.length line - half_len)))
            + acc)
            file
      | _ -> acc
    in
    read_aux 0 file
  in
  print_endline "Day 3 Part 1: ";
  print_int (read_file input);
  print_newline ()

let part_2 (input : in_channel) =
  let get_char_code c =
    match c with
    | a when a >= 'a' && a <= 'z' -> Char.code a - Char.code 'a' + 1
    | a when a >= 'A' && a <= 'Z' -> Char.code a - Char.code 'A' + 27
    | _ -> 0
  in
  let remove_whitespace erase =
    let b = Buffer.create (String.length erase) in
    String.iter (fun c -> if c = ' ' then () else Buffer.add_char b c) erase;
    Buffer.contents b
  in
  let find_common s1 s2 s3 =
    let common_aux s1 s2 =
      remove_whitespace
        (String.map
           (fun c ->
             match String.index_opt s1 c with Some _ -> c | None -> ' ')
           s2)
    in
    (common_aux s1 (common_aux s2 s3)).[0]
  in
  let read_file file =
    let rec read_aux i acc lines file =
      match input_line file with
      | exception End_of_file -> acc
      | line when String.length line > 2 ->
          if i = 2 then
            let line_1, line_2 =
              match lines with [ a; b ] -> (a, b) | _ -> ("", "")
            in
            read_aux 0
              (get_char_code (find_common line line_1 line_2) + acc)
              [] file
          else read_aux (i + 1) acc (line :: lines) file
      | _ -> acc
    in
    read_aux 0 0 [] file
  in
  print_endline "Part 2: ";
  print_int (read_file input)
