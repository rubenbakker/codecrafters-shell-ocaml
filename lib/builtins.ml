open! Base
open Lwt.Infix

let exit () = Unix._exit 0

let echo (args : Cmdargs.t) =
  let output =
    Stdlib.Printf.sprintf "%s" (String.concat ~sep:" " (List.tl_exn args.args))
  in
  match args.stdout with
  | Some filename ->
      Lwt_io.with_file filename ~mode:Lwt_io.Output (fun f ->
          Lwt_io.fprintl f output)
  | None -> Lwt_io.printl output

let type_ arg =
  match arg with
  | "exit" | "echo" | "type" | "pwd" ->
      Lwt_io.printlf "%s is a shell builtin" arg
  | _ -> (
      match Executable.search_path arg with
      | Some path -> Lwt_io.printlf "%s is %s" arg path
      | None -> Lwt_io.printlf "%s: not found" arg)

let pwd () = Lwt_io.printl (Unix.getcwd ())

let cd path =
  let path = match path with "~" -> Unix.getenv "HOME" | _ -> path in
  if Stdlib.Sys.file_exists path && Stdlib.Sys.is_directory path then (
    Unix.chdir path;
    Lwt.return_unit)
  else Lwt_io.printlf "cd: %s: No such file or directory" path
