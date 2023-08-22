module Day4 = struct
    type t = (int * int) * (int * int)

    (** converts a string of format 'x-y,z-w' to a tuple (x,y), (z,w) where x, y , z and w are ints *)
    let of_string = function | s -> Scanf.sscanf s "%d-%d,%d-%d" (fun x y z w -> (x, y), (z, w))

    (** returns true if all of either tuple overlaps the other*)
    let over_lap_all ((x1,y1),(x2,y2)) = 
        let first_gap, second_gap = y1 - x1, y2 - x2 in 
        if first_gap >= second_gap then y1 >= y2 && x1 <= x2
        else y2 >= y1 && x2 <= x1

    (** returns true if any of either tuple overlaps the other*)
    let over_lap_any ((x1,y1),(x2,y2)) =
        let first_gap, second_gap = y1 - x1, y2 - x2 in 
        if first_gap >= second_gap then y1 >= y2 || x1 <= x2
        else y2 >= y1 || x2 <= x1
    
end
