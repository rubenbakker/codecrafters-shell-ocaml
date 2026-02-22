open! Base

let exit () = Unix._exit 0
let echo args = Lwt_io.printl (String.concat ~sep:" " args)
