open! Base

let exit () = Unix._exit 0
let echo args = Lwt_io.printl (String.concat ~sep:" " args)

let is_executable fullpath =
  try
    Unix.access fullpath [ Unix.X_OK ];
    true
  with Unix.Unix_error _ -> false

let path_for_executable executable_name =
  let path = Unix.getenv "PATH" |> String.split ~on:':' in
  List.map path ~f:(fun p ->
      let fullpath = Stdlib.Filename.concat p executable_name in
      if Stdlib.Sys.file_exists fullpath && is_executable fullpath then
        Some fullpath
      else None)
  |> List.filter_opt |> List.hd

let type_ arg =
  match arg with
  | "exit" | "echo" | "type" -> Lwt_io.printlf "%s is a shell builtin" arg
  | _ -> (
      match path_for_executable arg with
      | Some path -> Lwt_io.printlf "%s is %s" arg path
      | None -> Lwt_io.printlf "%s: not found" arg)
