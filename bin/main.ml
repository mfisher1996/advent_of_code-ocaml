open Advent_of_code_ocaml.Aoc2023
open Advent_of_code_ocaml

let () = Files.process "./prod_1" Day1.part_2
let () = print_endline "Day 2 Part 1: "
let () = Files.process "./prod_2" Day2.part_2

let () =
  let file = "./prod_2" in
  let input = open_in file in
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

let () =
  let file = "./prod_3" in
  let input = open_in file in
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

let () =
  let file = "./prod_3" in
  let input = open_in file in
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

open Advent_of_code_ocaml

let () =
  let file_n = "./prod_4" in
  let input = open_in file_n in
  let rec read_file acc file =
    match input_line file with
    | exception End_of_file -> acc
    | line when String.length line > 0 ->
        read_file
          (if Day4.over_lap_all (Day4.of_string line) then acc + 1 else acc)
          file
    | _ -> acc
  in
  print_endline "\nDay 4 Part 1";
  print_int (read_file 0 input);
  let rec read_file_2 acc file =
    match input_line file with
    | exception End_of_file -> acc
    | line when String.length line > 0 ->
        read_file_2
          (if Day4.over_lap_any (Day4.of_string line) then acc + 1 else acc)
          file
    | _ -> acc
  in
  let input = open_in file_n in
  print_endline "\nPart 2";
  print_int (read_file_2 0 input)
