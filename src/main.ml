open! Base

let ( let* ) = Lwt.bind

let repl () =
  let rec goo () =
    let* () = Lwt_io.print "$ " in
    let* line = Lwt_io.read_line Lwt_io.stdin in
    let* () = Lwt_io.printlf "%s: command not found" line in
    goo () in
  goo ()

let () =
  Lwt_main.run (repl ())
