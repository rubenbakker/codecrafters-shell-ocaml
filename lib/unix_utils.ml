let equal_file_descr left right = left = right
let write_string fd str = Unix.write_substring fd str 0 (String.length str) |> ignore
