open Base

let histfile unit =
  try Some (Unix.getenv "HISTFILE") with
  | Stdlib.Not_found -> None
;;

let read_history_file path history =
  In_channel.with_open_text path (fun inch -> In_channel.input_lines inch)
  |> fun lines ->
  history
  := List.concat
       [ lines |> List.filter ~f:(fun x -> not (String.is_empty x)) |> List.rev
       ; !history
       ]
;;

let write_history_file path history =
  Out_channel.with_open_text path (fun outch ->
    history
    |> List.rev
    |> List.map ~f:(fun line ->
      Out_channel.output_string outch (String.concat ~sep:"" [ line; "\n" ])))
  |> ignore
;;

let write_history_file path history =
  Out_channel.with_open_text path (fun outch ->
    history
    |> List.rev
    |> List.map ~f:(fun line ->
      Out_channel.output_string outch (String.concat ~sep:"" [ line; "\n" ]));
    0)
;;

let append_to_history_file path history =
  let existing_lines =
    In_channel.with_open_text path (fun inch -> In_channel.input_lines inch)
    |> List.filter ~f:(fun x -> not (String.is_empty x))
  in
  let rec filter_history lines acc =
    match lines with
    | line :: rest ->
      (match String.split line ~on:' ' with
       | [] -> filter_history rest acc
       | "history" :: "-a" :: _ when not (List.is_empty acc) -> acc
       | _ :: _ -> filter_history rest (line :: acc))
    | [] -> acc
  in
  let new_history_lines = filter_history history [] |> List.rev in
  let all_lines = List.concat [ new_history_lines; List.rev existing_lines ] in
  write_history_file path all_lines
;;

let print_history entries_from_end history stdout =
  let history_size = List.length history in
  let entries_from_end = Option.value entries_from_end ~default:history_size in
  history
  |> List.rev
  |> List.mapi ~f:(fun idx line ->
    if history_size - idx <= entries_from_end
    then Some (Stdlib.Printf.sprintf "    %d %s\n" (idx + 1) line)
    else None)
  |> List.filter_opt
  |> List.map ~f:(Unix_utils.write_string stdout)
  |> ignore
;;

let add_history line history =
  Readline.add_history line;
  history := line :: !history
;;

let init_with_histfile history =
  match histfile () with
  | Some hf -> if Stdlib.Sys.file_exists hf then read_history_file hf history
  | None -> ()
;;

let write_with_histfile history =
  match histfile () with
  | Some hf -> write_history_file hf history
  | None -> ()
;;
