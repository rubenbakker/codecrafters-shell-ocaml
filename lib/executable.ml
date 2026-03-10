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

type result_t = {
  stdin: Lwt_process.redirection;
  stdout: Lwt_process.redirection;
  process_status: Unix.process_status
}

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

let rec create_pipe (args : Cmdargs.t) =
  let in_ch, out_ch = Lwt_unix.pipe_out ~cloexec:true () in
  let _fork_result = Lwt_unix.fork () in
  exec args out_ch

and exec_with_redirection (args : Cmdargs.t) stdin stdout stderr =
  let command = List.hd_exn args.args in
  let args = List.tl_exn args.args in
  match search_path command with
  | Some path ->
    Lwt_process.exec
      ~stdin
      ~stdout
      ~stderr
      (path, List.to_array (Stdlib.Filename.basename path :: args))
    >>= (function process_status -> Lwt.return { stdin; stdout;  process_status}
  | None -> Lwt_io.printlf "%s: command not found" command

and exec (args : Cmdargs.t) out_ch =
  let stdin, stdout =
    match args.stdout with
    | Some (PipeStdout pipe_args) ->
      let _, out_ch = Lwt_unix.pipe_out ~cloexec:true () in
      match Lwt_unix.fork () with 
      | 0 -> `FD_move (Lwt_unix.unix_file_descr in_ch), 
      | n when 
      `FD_move (Lwt_unix.unix_file_descr out_ch)
    | Some (RedirectStdout redirect) ->
      let%bind stdout = Cmdargs.with_output (Some redirect) in
      `FD_keep, stdout
    | None -> `Keep
  in
  let%bind stderr = Cmdargs.with_output args.stderr in
  exec_with_redirection args stdin stdout stderr
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
