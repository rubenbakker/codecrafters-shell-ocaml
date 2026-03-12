open! Base

let completion prefix =
  Readline.Custom
    (List.concat [ Builtins.completions prefix; Executable.completions prefix ])
;;

let user_input prompt = Readline.readline ~prompt:"$ " ~completion_fun:completion ()

let repl () =
  Readline.init ();
  let rec loop () =
    let _ = Stdlib.flush_all () in
    match user_input "$ " with
    | None -> ()
    | Some line ->
      let args = Cmdargs.parse line in
      let first_args = List.hd_exn args in
      (match first_args.args with
       | "exit" :: [] -> Builtins.exit ()
       | "echo" :: _rest ->
         Builtins.echo first_args;
         loop ()
       | [ "type"; arg ] ->
         Builtins.type_ arg;
         loop ()
       | [ "pwd" ] ->
         Builtins.pwd ();
         loop ()
       | [ "cd"; path ] ->
         Builtins.cd path;
         loop ()
       | _command :: _rest ->
         Executable.run_pipeline args;
         loop ()
       | _ ->
         Stdlib.Printf.printf "%s: command not found\n" line;
         loop ())
  in
  loop ()
;;
