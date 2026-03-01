open! Base
open Lib

let () = Lwt_main.run (Repl.repl ())
