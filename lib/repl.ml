open! Base

let repl () =
  let rec go () =
    let open Lwt.Let_syntax in
    let%bind () = Lwt_io.print "$ " in
    let%bind line = Lwt_io.read_line Lwt_io.stdin in
    let args = String.split ~on:' ' line in
    match args with
    | "exit" :: [] -> Builtins.exit ()
    | "echo" :: rest ->
        let%bind () = Builtins.echo rest in
        go ()
    | [ "type"; arg ] ->
        let%bind () = Builtins.type_ arg in
        go ()
    | _ ->
        let%bind () = Lwt_io.printlf "%s: command not found" line in
        go ()
  in
  go ()
