open! Base
open Lwt.Infix
open Lwt.Let_syntax

let completion str = Readline.Custom [ "exit", ' '; "echo", ' ' ]

let user_input prompt =
  Lwt.return (Readline.readline ~prompt:"$ " ~completion_fun:completion ())
;;

let repl () =
  Readline.init ();
  let rec go () =
    let%bind _ = Lwt_io.flush_all () in
    match%bind user_input "$ " with
    | None -> Lwt.return_unit
    | Some line ->
      let args = Cmdargs.parse line in
      (match args.args with
       | "exit" :: [] -> Builtins.exit ()
       | "echo" :: _rest -> Builtins.echo args >>= go
       | [ "type"; arg ] -> Builtins.type_ arg >>= go
       | [ "pwd" ] -> Builtins.pwd () >>= go
       | [ "cd"; path ] -> Builtins.cd path >>= go
       | _command :: _rest -> Executable.exec args >>= Lwt_io.flush_all >>= go
       | _ -> Lwt_io.printlf "%s: command not found" line >>= go)
  in
  go ()
;;
