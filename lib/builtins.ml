open! Base

let exit () = Unix._exit 0
let echo args = Lwt_io.printl (String.concat ~sep:" " args)

let type_ arg =
  match arg with
  | "exit" | "echo" | "type" -> Lwt_io.printlf "%s is a shell builtin" arg
  | _ -> (
      match Executable.full_path arg with
      | Some path -> Lwt_io.printlf "%s is %s" arg path
      | None -> Lwt_io.printlf "%s: not found" arg)
