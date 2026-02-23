open! Base

type scanner_state_t = Normal | SingleQuote

let rec scan state chars acc args =
  match (state, chars) with
  | SingleQuote, '\'' :: '\'' :: rest -> scan SingleQuote rest acc args
  | SingleQuote, '\'' :: rest -> scan Normal rest [] (acc :: args)
  | SingleQuote, char :: rest ->
      scan SingleQuote rest (char :: acc) (acc :: args)
  | Normal, '\'' :: rest -> scan SingleQuote rest [] args
  | Normal, char :: rest when Char.(char = ' ' || char = '\t') -> (
      match acc with [] -> scan Normal rest acc args)
  | Normal, char :: rest -> scan Normal rest (char :: acc) args
  | _, [] -> List.rev (acc :: args)

let parse line =
  scan Normal (line |> String.strip |> String.to_array |> Array.to_list) [] []
