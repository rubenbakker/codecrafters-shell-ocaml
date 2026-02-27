open! Base
open Lwt.Let_syntax
open Lwt.Infix

let is_executable fullpath =
  try
    Unix.access fullpath [ Unix.X_OK ];
    true
  with Unix.Unix_error _ -> false

let search_path executable_name =
  let path = Unix.getenv "PATH" |> String.split ~on:':' in
  List.map path ~f:(fun p ->
      let fullpath = Stdlib.Filename.concat p executable_name in
      if Stdlib.Sys.file_exists fullpath && is_executable fullpath then
        Some fullpath
      else None)
  |> List.filter_opt |> List.hd

let exec (args : Cmdargs.t) =
  let%bind stdout = Cmdargs.with_stdout args.redirect in
  let command = List.hd_exn args.args in
  match search_path command with
  | Some path -> (
      Lwt_process.exec ~stdout
        (path, List.to_array (Stdlib.Filename.basename command :: args.args))
      >>= function
      | WEXITED 0 -> Lwt.return_unit
      | _ -> Lwt_io.printf "Error executing command.")
  | None -> Lwt_io.printlf "%s: command not found" command
