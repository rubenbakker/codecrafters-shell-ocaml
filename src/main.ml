open! Base

let ( let* ) = Lwt.bind

let repl () =
    let* () = Lwt_io.printl "$ " in
    let* line = Lwt_io.read_line Lwt_io.stdin in
    Lwt_io.printlf "%s: command not found" line

let () =
  Lwt_main.run (repl ())
