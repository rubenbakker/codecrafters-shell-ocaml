open! Base

let all = [ "exit"; "echo"; "type"; "pwd"; "history"; "jobs" ]

let completions prefix =
  all |> List.filter ~f:(String.is_prefix ~prefix) |> List.map ~f:(fun item -> item, ' ')
;;
