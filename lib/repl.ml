open! Base
open Lwt.Infix

let repl () =
  let rec go () =
    let open Lwt.Let_syntax in
    let%bind () = Lwt_io.print "$ " in
    let%bind line = Lwt_io.read_line Lwt_io.stdin in
    let args = Cmdargs.parse line in
    match args.args with
    | "exit" :: [] -> Builtins.exit ()
    | "echo" :: rest -> Builtins.echo rest >>= go
    | [ "type"; arg ] -> Builtins.type_ arg >>= go
    | [ "pwd" ] -> Builtins.pwd () >>= go
    | [ "cd"; path ] -> Builtins.cd path >>= go
    | command :: _rest -> Executable.exec command args >>= go
    | _ -> Lwt_io.printlf "%s: command not found" line >>= go
  in
  go ()
