open! Base

let completion prefix =
  Readline.Custom
    (List.concat [ Builtins.completions prefix; Executable.completions prefix ])
;;

let user_input prompt = Readline.readline ~prompt:"$ " ~completion_fun:completion ()

let repl () =
  Readline.init ();
  let history = ref [] in
  History.init_with_histfile history;
  let rec loop () =
    let _ = Stdlib.flush_all () in
    match user_input "$ " with
    | None -> ()
    | Some line ->
      let args = Cmdargs.parse line in
      History.add_history line history;
      Executable.run_pipeline args history;
      loop ()
  in
  loop ()
;;
