open! Base
open Lib

let repl () =
  let rec go () =
    let open Lwt.Let_syntax in
    let%bind () = Lwt_io.print "$ " in
    let%bind line = Lwt_io.read_line Lwt_io.stdin in
    match String.lowercase line with 
    | "exit" -> Builtins.exit ()
    | _ -> let%bind () = Lwt_io.printlf "%s: command not found" line in
    go () in
  go ()

let () =
  Lwt_main.run (repl ())
