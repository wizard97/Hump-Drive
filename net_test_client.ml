open Filetransfer
open Async


let _ = client_connect_read "10.132.7.82" "newfox.jpeg"
let _ = Scheduler.go ()
(* let _ = run_server () *)
