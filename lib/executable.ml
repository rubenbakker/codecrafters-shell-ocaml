open! Base
open Lwt.Let_syntax
open Lwt.Infix

let is_executable fullpath =
  try
    Unix.access fullpath [ Unix.X_OK ];
    true
  with
  | Unix.Unix_error _ -> false
;;

let path_list unit = Unix.getenv "PATH" |> String.split ~on:':'

let search_path executable_name =
  let path = path_list () in
  List.map path ~f:(fun p ->
    let fullpath = Stdlib.Filename.concat p executable_name in
    if Stdlib.Sys.file_exists fullpath && is_executable fullpath
    then Some fullpath
    else None)
  |> List.filter_opt
  |> List.hd
;;

let exec (args : Cmdargs.t) =
  let%bind stdout = Cmdargs.with_output args.stdout in
  let%bind stderr = Cmdargs.with_output args.stderr in
  let command = List.hd_exn args.args in
  let args = List.tl_exn args.args in
  match search_path command with
  | Some path ->
    Lwt_process.exec
      ~stdout
      ~stderr
      (path, List.to_array (Stdlib.Filename.basename path :: args))
    >>= (function
     | WEXITED 0 -> Lwt.return_unit
     | _ -> Lwt.return_unit)
  | None -> Lwt_io.printlf "%s: command not found" command
;;

let completions prefix : (string * char) list =
  path_list ()
  |> List.filter ~f:Stdlib.Sys.file_exists
  |> List.filter ~f:Stdlib.Sys.is_directory
  |> List.map ~f:(fun dir ->
    Stdlib.Sys.readdir dir
    |> Array.to_list
    |> List.filter ~f:(String.is_prefix ~prefix)
    |> List.map ~f:(fun item -> item, ' '))
  |> List.concat
;;
