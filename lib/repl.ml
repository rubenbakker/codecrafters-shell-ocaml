open! Base
open Lwt.Infix
open Lwt.Let_syntax

let user_input prompt =
  match LNoise.linenoise prompt with
  | Some result -> Lwt.return result
  | None -> Lwt.return ""
;;

let setup_completion () =
  LNoise.set_completion_callback (fun line_so_far ln_completions ->
    if String.is_empty line_so_far
    then ()
    else
      List.filter Builtins.all ~f:(fun builtin ->
        String.is_prefix ~prefix:line_so_far builtin)
      |> List.map ~f:(fun completion -> String.append completion " ")
      |> List.iter ~f:(LNoise.add_completion ln_completions));
  ()
;;

let repl () =
  setup_completion ();
  let rec go () =
    let%bind _ = Lwt_io.flush_all () in
    let%bind line = user_input "$ " in
    let args = Cmdargs.parse line in
    match args.args with
    | "exit" :: [] -> Builtins.exit ()
    | "echo" :: _rest -> Builtins.echo args >>= go
    | "jobs" :: [] -> Builtins.jobs () >>= go
    | [ "type"; arg ] -> Builtins.type_ arg >>= go
    | [ "pwd" ] -> Builtins.pwd () >>= go
    | [ "cd"; path ] -> Builtins.cd path >>= go
    | _command :: _rest -> Executable.exec args >>= Lwt_io.flush_all >>= go
    | _ -> Lwt_io.printlf "%s: command not found" line >>= go
  in
  go ()
;;
