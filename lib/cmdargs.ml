open! Base

type t =
  { args : string list
  ; stdout : redirect_t option
  ; stderr : redirect_t option
  }

and redirect_t =
  { path : string
  ; append : bool
  }
[@@deriving sexp, compare, equal]

type scanner_state_t =
  | Normal
  | SingleQuote
  | DoubleQuote

let rec scan state chars acc args =
  let is_whitespace_char char = Char.(char = ' ' || char = '\t') in
  let add_arg acc args =
    if not (List.is_empty acc)
    then (List.rev acc |> String.of_char_list) :: args
    else args
  in
  match state, chars with
  | SingleQuote, '\'' :: '\'' :: rest -> scan SingleQuote rest acc args
  | SingleQuote, '\'' :: rest -> scan Normal rest [] (add_arg acc args)
  | SingleQuote, char :: rest -> scan SingleQuote rest (char :: acc) args
  | DoubleQuote, '"' :: '"' :: rest -> scan DoubleQuote rest acc args
  | DoubleQuote, '"' :: char :: rest
    when (not (is_whitespace_char char)) && List.length acc > 0 ->
    scan Normal rest (char :: acc) args
  | DoubleQuote, '"' :: rest -> scan Normal rest [] (add_arg acc args)
  | DoubleQuote, '\\' :: char :: rest
    when Char.(char = '$' || char = '\\' || char = '"' || char = '`' || char = '\n') ->
    scan DoubleQuote rest (char :: acc) args
  | DoubleQuote, char :: rest -> scan DoubleQuote rest (char :: acc) args
  | Normal, '>' :: '>' :: rest | Normal, '1' :: '>' :: '>' :: rest ->
    scan Normal rest [] ("1>>" :: add_arg acc args)
  | Normal, '>' :: rest | Normal, '1' :: '>' :: rest ->
    scan Normal rest [] ("1>" :: add_arg acc args)
  | Normal, '2' :: '>' :: '>' :: rest -> scan Normal rest [] ("2>>" :: add_arg acc args)
  | Normal, '2' :: '>' :: rest -> scan Normal rest [] ("2>" :: add_arg acc args)
  | Normal, '|' :: rest -> scan Normal rest [] ("|" :: add_arg acc args)
  | Normal, '\'' :: '\'' :: rest -> scan Normal rest acc args
  | Normal, '\'' :: rest -> scan SingleQuote rest acc args
  | Normal, '"' :: '"' :: rest -> scan Normal rest acc args
  | Normal, '"' :: rest -> scan DoubleQuote rest acc args
  | Normal, '\\' :: char :: rest -> scan Normal rest (char :: acc) args
  | Normal, char :: rest when is_whitespace_char char && List.length acc > 0 ->
    scan Normal rest [] (add_arg acc args)
  | Normal, char :: rest when Char.(char = ' ' || char = '\t') ->
    scan Normal rest acc args
  | Normal, char :: rest -> scan Normal rest (char :: acc) args
  | _, [] -> List.rev (if List.length acc > 0 then add_arg acc args else args)
;;

let rec prepare_args args =
  let rec loop args arg_acc pipeline_acc stdout stderr =
    match args with
    | [] -> List.rev ({ args = List.rev arg_acc; stdout; stderr } :: pipeline_acc)
    | "|" :: rest ->
      { args = List.rev arg_acc; stdout = None; stderr }
      :: loop rest [] pipeline_acc None None
    | "1>" :: filename :: rest ->
      loop rest arg_acc pipeline_acc (Some { path = filename; append = false }) stderr
    | "1>>" :: filename :: rest ->
      loop rest arg_acc pipeline_acc (Some { path = filename; append = true }) stderr
    | "2>" :: filename :: rest ->
      loop rest arg_acc pipeline_acc stdout (Some { path = filename; append = false })
    | "2>>" :: filename :: rest ->
      loop rest arg_acc pipeline_acc stdout (Some { path = filename; append = true })
    | arg :: rest -> loop rest (arg :: arg_acc) pipeline_acc stdout stderr
  in
  loop args [] [] None None
;;

let parse line =
  scan Normal (line |> String.strip |> String.to_array |> Array.to_list) [] []
  |> prepare_args
;;

let open_flags path append =
  let file_exits = Stdlib.Sys.file_exists path in
  let open Unix in
  match file_exits, append with
  | true, true -> [ O_APPEND; O_WRONLY; O_CLOEXEC ]
  | _ -> [ O_CREAT; O_WRONLY; O_CLOEXEC ]
;;

let with_output redirect default_value =
  match redirect with
  | Some redirect ->
    let options = open_flags redirect.path redirect.append in
    Unix.openfile redirect.path options 0o644
  | None -> default_value
;;

let is_executable prefix =
  let the_end = String.suffix prefix 1 in
  let ends_with_space = String.(the_end = " ") in
  match parse prefix |> List.last with
  | None -> true
  | Some arg -> List.length arg.args < 2 && not ends_with_space
;;
