open Unix

let run_pipeline cmds =
  let rec loop prev_read pids = function
    | [] -> pids
    | [ cmd ] ->
      (* letztes Programm *)
      let pid = create_process cmd.(0) cmd prev_read stdout stderr in
      pid :: pids
    | cmd :: rest ->
      let read_end, write_end = pipe () in
      let pid = create_process cmd.(0) cmd prev_read write_end stderr in
      close write_end;
      if prev_read <> stdin then close prev_read;
      loop read_end (pid :: pids) rest
  in
  let pids = loop stdin [] cmds in
  List.iter (fun pid -> ignore (waitpid [] pid)) pids
;;
