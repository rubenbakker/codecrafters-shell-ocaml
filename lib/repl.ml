open! Base

let completion prefix =
  Readline.Custom
    (List.concat [ Builtins.completions prefix; Executable.completions prefix ])
;;

let user_input prompt = Readline.readline ~prompt:"$ " ~completion_fun:completion ()

let repl () =
  Readline.init ();
  let rec loop history =
    let _ = Stdlib.flush_all () in
    match user_input "$ " with
    | None -> ()
    | Some line ->
      Readline.add_history line;
      let args = Cmdargs.parse line in
      Executable.run_pipeline args history;
      loop (line :: history)
  in
  loop []
;;
