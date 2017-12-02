
open Filetransfer



let run_client () =
  let c = client_connect "127.0.0.1" in
  client_get_file c "test.txt" "result.txt"
  (* Reads first file, saved it locally as the second *)

let run_server () =
  let s = create_server () in
    server_add_file "test.txt"

let _ = run_client ()
(* let _ = run_server () *)