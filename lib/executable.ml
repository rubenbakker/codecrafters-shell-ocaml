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

type result_t =
  { stdin : Lwt_process.redirection
  ; stdout : Lwt_process.redirection
  ; process_status : Unix.process_status
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

let rec launch ?(input : Unix.file_descr option) (args : Cmdargs.t) =
  let stdin =
    match input with
    | Some input -> `FD_move input
    | None -> `Keep
  in
  let%bind stderr = Cmdargs.with_output args.stderr in
  match args.stdout with
  | Some (PipeStdout pipe_args) ->
    let in_ch, out_ch = Lwt_unix.pipe ~cloexec:true () in
    let out_ch = Lwt_unix.unix_file_descr out_ch in
    (match Lwt_unix.fork () with
     | 0 -> launch ~input:(Lwt_unix.unix_file_descr in_ch) pipe_args
     | n when n > 0 -> exec_with_redirection args stdin (`FD_move out_ch) stderr
     | _ -> exec_with_redirection args stdin (`FD_move out_ch) stderr)
  | Some (RedirectStdout redirect) ->
    let%bind stdout = Cmdargs.with_output (Some redirect) in
    exec_with_redirection args stdin stdout stderr
  | None -> exec_with_redirection args stdin `Keep stderr

and exec_with_redirection
      (args : Cmdargs.t)
      (stdin : Lwt_process.redirection)
      (stdout : Lwt_process.redirection)
      (stderr : Lwt_process.redirection)
  =
  let command = List.hd_exn args.args in
  let args = List.tl_exn args.args in
  match search_path command with
  | Some path ->
    Lwt_process.exec
      ~stdin
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
