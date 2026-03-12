open! Base

let exit () = Unix._exit 0

let write_string
      (redirect : Cmdargs.redirect_t option)
      (default_fd : Unix.file_descr)
      (output : string)
  =
  let fd = Cmdargs.with_output redirect default_fd in
  Unix.write_substring fd output 0 (String.length output) |> ignore;
  if not (Unix_utils.equal_file_descr default_fd fd) then Unix.close fd
;;

let echo (args : Cmdargs.t) =
  let output =
    Stdlib.Printf.sprintf "%s\n" (String.concat ~sep:" " (List.tl_exn args.args))
  in
  write_string args.stdout Unix.stdout output |> ignore;
  write_string args.stderr Unix.stderr ""
;;

let all = [ "exit"; "echo"; "type"; "pwd" ]

let type_ arg =
  match arg with
  | "exit" | "echo" | "type" | "pwd" -> Stdlib.Printf.printf "%s is a shell builtin\n" arg
  | _ ->
    (match Executable.search_path arg with
     | Some path -> Stdlib.Printf.printf "%s is %s\n" arg path
     | None -> Stdlib.Printf.printf "%s: not found\n" arg)
;;

let pwd () = Stdlib.Printf.printf "%s\n" (Unix.getcwd ())

let cd path =
  let path =
    match path with
    | "~" -> Unix.getenv "HOME"
    | _ -> path
  in
  if Stdlib.Sys.file_exists path && Stdlib.Sys.is_directory path
  then Unix.chdir path
  else Stdlib.Printf.printf "cd: %s: No such file or directory\n" path
;;

let completions prefix =
  all |> List.filter ~f:(String.is_prefix ~prefix) |> List.map ~f:(fun item -> item, ' ')
;;
