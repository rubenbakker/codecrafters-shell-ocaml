open! Base

let exit () = Unix._exit 0
let echo args = Lwt_io.printl (String.concat ~sep:" " args)

let type_ arg =
  match arg with
  | "exit" | "echo" | "type" -> Lwt_io.printlf "%s is a shell builtin" arg
  | _ -> Lwt_io.printlf "%s: not found" arg
