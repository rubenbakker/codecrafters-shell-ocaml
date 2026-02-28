open! Base
open Lwt.Infix

type t = { args : string list; stdout : string option; stderr : string option }
[@@deriving sexp, compare, equal]

type scanner_state_t = Normal | SingleQuote | DoubleQuote

let rec scan state chars acc args =
  let is_whitespace_char char = Char.(char = ' ' || char = '\t') in
  let add_arg acc args =
    if not (List.is_empty acc) then
      (List.rev acc |> String.of_char_list) :: args
    else args
  in
  match (state, chars) with
  | SingleQuote, '\'' :: '\'' :: rest -> scan SingleQuote rest acc args
  | SingleQuote, '\'' :: rest -> scan Normal rest [] (add_arg acc args)
  | SingleQuote, char :: rest -> scan SingleQuote rest (char :: acc) args
  | DoubleQuote, '"' :: '"' :: rest -> scan DoubleQuote rest acc args
  | DoubleQuote, '"' :: char :: rest
    when (not (is_whitespace_char char)) && List.length acc > 0 ->
      scan Normal rest (char :: acc) args
  | DoubleQuote, '"' :: rest -> scan Normal rest [] (add_arg acc args)
  | DoubleQuote, '\\' :: char :: rest
    when Char.(
           char = '$' || char = '\\' || char = '"' || char = '`' || char = '\n')
    ->
      scan DoubleQuote rest (char :: acc) args
  | DoubleQuote, char :: rest -> scan DoubleQuote rest (char :: acc) args
  | Normal, '>' :: rest | Normal, '1' :: '>' :: rest ->
      scan Normal rest [] (">1" :: add_arg acc args)
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

let prepare_args args =
  let rec loop args acc stdout stderr =
    match args with
    | [] -> { args = List.rev acc; stdout; stderr }
    | ">1" :: filename :: rest -> loop rest acc (Some filename) stderr
    | ">2" :: filename :: rest -> loop rest acc stdout (Some filename)
    | arg :: rest -> loop rest (arg :: acc) stdout stderr
  in
  loop args [] None None

let parse line =
  scan Normal (line |> String.strip |> String.to_array |> Array.to_list) [] []
  |> prepare_args

let with_output filename =
  match filename with
  | Some filename ->
      Lwt_unix.openfile filename [ O_CREAT; O_WRONLY; O_CLOEXEC ] 0
      >|= fun fd -> `FD_move (Lwt_unix.unix_file_descr fd)
  | None -> Lwt.return `Keep
