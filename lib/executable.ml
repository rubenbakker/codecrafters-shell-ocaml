let compare_file_descr left right = left = right

open! Base

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

let run_command
      (args : Cmdargs.t)
      (stdin : Unix.file_descr)
      (stdout : Unix.file_descr)
      (stderr : Unix.file_descr)
  =
  let command = List.hd_exn args.args in
  Unix.create_process command (List.to_array args.args) stdin stdout stderr
;;

let run_pipeline (pipeline : Cmdargs.t list) =
  let open Cmdargs in
  let rec loop prev_read pids = function
    | [] -> pids
    | args :: [] ->
      let stdout = with_output args.stdout Unix.stdout in
      let stderr = with_output args.stderr Unix.stderr in
      let pid = run_command args prev_read stdout stderr in
      pid :: pids
    | args :: rest ->
      let read_end, write_end = Unix.pipe () in
      let stderr = with_output args.stderr Unix.stderr in
      let pid = run_command args prev_read write_end stderr in
      Unix.close write_end;
      if compare_file_descr prev_read Unix.stdin then Unix.close prev_read;
      loop read_end (pid :: pids) rest
  in
  let pids = loop Unix.stdin [] pipeline in
  Unix.waitpid [] (List.hd_exn pids) |> ignore;
  List.map pids ~f:Unix.kill |> ignore
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
