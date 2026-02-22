open! Base

let is_executable fullpath =
  try
    Unix.access fullpath [ Unix.X_OK ];
    true
  with Unix.Unix_error _ -> false

let full_path executable_name =
  let path = Unix.getenv "PATH" |> String.split ~on:':' in
  List.map path ~f:(fun p ->
      let fullpath = Stdlib.Filename.concat p executable_name in
      if Stdlib.Sys.file_exists fullpath && is_executable fullpath then
        Some fullpath
      else None)
  |> List.filter_opt |> List.hd

let exec command args =
  match full_path command with
  | Some path ->
      Unix.system (command :: args |> String.concat ~sep:" ") |> ignore;
      Lwt.return_unit
  | None -> Lwt_io.printlf "%s: command not found" command
