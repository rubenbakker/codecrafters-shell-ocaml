open! Base
open Lwt.Infix

let user_input prompt =
  match LNoise.linenoise prompt with
  | Some result -> Lwt.return result
  | None -> Lwt.return ""

let setup_completion () =
  LNoise.set_completion_callback (fun line_so_far ln_completions ->
      List.filter Builtins.all ~f:(fun builtin ->
          String.is_prefix ~prefix:line_so_far builtin)
      |> List.iter ~f:(LNoise.add_completion ln_completions));
  ()

let repl () =
  setup_completion ();
  let rec go () =
    let open Lwt.Let_syntax in
    let%bind _ = Lwt_io.flush_all () in
    let%bind line = user_input "$ " in
    let args = Cmdargs.parse line in
    match args.args with
    | "exit" :: [] -> Builtins.exit ()
    | "echo" :: _rest -> Builtins.echo args >>= go
    | [ "type"; arg ] -> Builtins.type_ arg >>= go
    | [ "pwd" ] -> Builtins.pwd () >>= go
    | [ "cd"; path ] -> Builtins.cd path >>= go
    | _command :: _rest -> Executable.exec args >>= go
    | _ -> Lwt_io.printlf "%s: command not found" line >>= go
  in
  go ()
