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
  (* let _ = Communication.start_server notify_callback in *)
  (* Send garbage state *)
  let _ = Communication.send_state peer "Dummy" (fun () -> print_string "Success" ) in
  Scheduler.go ()


let _ = main ()
