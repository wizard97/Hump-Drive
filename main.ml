(* NOTE main ties together all the parts of our system *)

(* Uses communicator to discover other peers *)
(* Establishes a connection via communicator *)
(* If differences on either this machine or the other, transfer handles updates *)
(* GUI displays all stuff *)
(* Crypto encrpyts files and user info/ connection-establishing processes *)
open Filetransfer
open Communication
open Async



let notify_callback peer msg =
  match msg with
  | State s -> print_string "Got state update!"; ()
  | Filerequest f ->
    print_string "Got request for file!";
    let _ = Filetransfer.create_server f in
    ()

let main () =
  let peer = {ip="127.0.0.1"; key="123abc"} in
  print_string "Testing123";
  Communication.start_server notify_callback
  let _ = Scheduler.go ()
