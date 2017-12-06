open Communication
open Async



let notify_callback cstate peer msg =
  match msg with
  | State s -> print_string "Got state update!"; Async.Deferred.return ()
  | Filerequest f ->
    print_string "Got request for file!";
    Communication.transfer_file f cstate


let main () =
  let peer = {ip="127.0.0.1"; key=Crypto.key_from_string""} in
  print_string "Testing123";
  let _ = Communication.start_server notify_callback in
  Scheduler.go ()


let _ = main ()
