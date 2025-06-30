type t = (int * int) * (int * int)

(** converts a string of format 'x-y,z-w' to a tuple (x,y), (z,w) where x, y , z
    and w are ints *)
let of_string = function
  | s -> Scanf.sscanf s "%d-%d,%d-%d" (fun x y z w -> ((x, y), (z, w)))

(** returns true if all of either tuple overlaps the other*)
let over_lap_all ((x1, y1), (x2, y2)) =
  if y1 - x1 >= y2 - x2 then y1 >= y2 && x1 <= x2 else y2 >= y1 && x2 <= x1

(** returns true if any of either tuple overlaps the other*)
let over_lap_any ((x1, y1), (x2, y2)) =
  (x1 <= x2 && y1 >= x2) || (x2 <= x1 && y2 >= x1)

let part_2 (input : in_channel) =
  let rec read_file acc file =
    match input_line file with
    | exception End_of_file -> acc
    | line when String.length line > 0 ->
        read_file (if over_lap_all (of_string line) then acc + 1 else acc) file
    | _ -> acc
  in
  print_endline "\nDay 4 Part 1";
  print_int (read_file 0 input);
  let rec read_file_2 acc file =
    match input_line file with
    | exception End_of_file -> acc
    | line when String.length line > 0 ->
        read_file_2
          (if over_lap_any (of_string line) then acc + 1 else acc)
          file
    | _ -> acc
  in
  print_endline "\nPart 2";
  print_int (read_file_2 0 input)
