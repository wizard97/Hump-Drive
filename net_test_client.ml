open Filetransfer
open Async

let run_client () =
  let c = client_connect "127.0.0.1" in
  client_get_file c "test.txt" "result.txt"
  (* Reads first file, saved it locally as the second *)

let run_server () =
  let s = create_server () in
    server_add_file "war_peace.txt"

let _ = run_server ()
let _ = Scheduler.go ()
(* let _ = run_server () *)
