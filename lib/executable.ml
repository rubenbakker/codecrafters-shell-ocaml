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

let write_string fd str = Unix.write_substring fd str 0 (String.length str) |> ignore

let echo_builtin args stdout =
  Stdlib.Printf.sprintf "%s\n" (String.concat ~sep:" " (List.tl_exn args))
  |> write_string stdout
;;

let history_builtin (history : string list) stdout =
  "history" :: history
  |> List.rev
  |> List.mapi ~f:(fun idx line -> Stdlib.Printf.sprintf "%d %s\n" (idx + 1) line)
  |> List.map ~f:(write_string stdout)
  |> ignore
;;

let type_builtin args stdout =
  let arg =
    match args with
    | [ "type"; arg ] -> arg
    | _ -> "type"
  in
  match arg with
  | "exit" | "echo" | "type" | "pwd" | "history" ->
    Stdlib.Printf.sprintf "%s is a shell builtin\n" arg |> write_string stdout
  | _ ->
    (match search_path arg with
     | Some path -> Stdlib.Printf.sprintf "%s is %s\n" arg path
     | None -> Stdlib.Printf.sprintf "%s: not found\n" arg)
    |> write_string stdout
;;

let pwd_builtin stdout =
  Stdlib.Printf.sprintf "%s\n" (Unix.getcwd ()) |> write_string stdout
;;

let exit_builtin () = Unix._exit 0

let cd_builtin args stdout =
  let path =
    match args with
    | [ "cd"; path ] -> path
    | [ "cd" ] -> "~"
    | _ -> "."
  in
  let path =
    match path with
    | "~" -> Unix.getenv "HOME"
    | _ -> path
  in
  if Stdlib.Sys.file_exists path && Stdlib.Sys.is_directory path
  then Unix.chdir path
  else
    Stdlib.Printf.sprintf "cd: %s: No such file or directory\n" path
    |> write_string stdout
;;

let run_command
      (args : Cmdargs.t)
      (stdin : Unix.file_descr)
      (stdout : Unix.file_descr)
      (stderr : Unix.file_descr)
      (history : string list)
  =
  let stdout = Cmdargs.with_output args.stdout stdout in
  let stderr = Cmdargs.with_output args.stderr stderr in
  let command = List.hd_exn args.args in
  match command with
  | "echo" ->
    echo_builtin args.args stdout;
    0
  | "type" ->
    type_builtin args.args stdout;
    0
  | "pwd" ->
    pwd_builtin stdout;
    0
  | "cd" ->
    cd_builtin args.args stdout;
    0
  | "history" ->
    history_builtin history stdout;
    0
  | "exit" -> exit_builtin ()
  | _ ->
    (match command |> search_path with
     | Some command ->
       Unix.create_process command (List.to_array args.args) stdin stdout stderr
     | None ->
       Stdlib.Printf.printf "%s: command not found\n" command;
       -1)
;;

let run_pipeline (pipeline : Cmdargs.t list) (history : string list) =
  let open Cmdargs in
  let rec loop prev_read pids = function
    | [] -> pids
    | args :: [] ->
      let pid = run_command args prev_read Unix.stdout Unix.stderr history in
      pid :: pids
    | args :: rest ->
      let read_end, write_end = Unix.pipe () in
      let stderr = with_output args.stderr Unix.stderr in
      let pid = run_command args prev_read write_end stderr history in
      Unix.close write_end;
      if not (Unix_utils.equal_file_descr Unix.stdin prev_read) then Unix.close prev_read;
      loop read_end (pid :: pids) rest
  in
  let pids = loop Unix.stdin [] pipeline in
  match List.hd_exn pids with
  | -1 | 0 -> ()
  | pid ->
    Unix.waitpid [] pid |> ignore;
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
