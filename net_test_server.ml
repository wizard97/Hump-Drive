open Filetransfer
open Async


let _ = create_server "fox.jpeg"
let _ = Scheduler.go ()
(* let _ = run_server () *)
