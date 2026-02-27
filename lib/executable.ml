open! Base
open Lwt.Let_syntax
open Lwt.Infix

type t = { args : string list; redirect : string option }

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

let prepare_args args =
  let rec loop args acc redirect =
    match args with
    | [] -> { args = List.rev args; redirect }
    | ">1" :: filename :: rest -> loop rest acc (Some filename)
    | arg :: rest -> loop rest (arg :: acc) redirect
  in
  loop args [] None

let with_stdout filename =
  match filename with
  | Some filename ->
      let%bind fd = Lwt_unix.openfile filename [ O_WRONLY; O_CLOEXEC ] 0 in
      Lwt.return (`FD_move (Lwt_unix.unix_file_descr fd))
  | None -> Lwt.return `Keep

let exec command args =
  let args = prepare_args args in
  let%bind stdout = with_stdout args.redirect in
  match search_path command with
  | Some path -> (
      Lwt_process.exec ~stdout
        (path, List.to_array (Stdlib.Filename.basename path :: args.args))
      >>= function
      | WEXITED 0 -> Lwt.return_unit
      | _ -> Lwt_io.printf "Error executing command.")
  | None -> Lwt_io.printlf "%s: command not found" command
