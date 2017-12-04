open Communication
open Async



let notify_callback cstate peer msg =
  match msg with
  | State s -> print_string "Got state update!"; Async.Deferred.return ()
  | Filerequest f ->
    print_string "Got request for file!";
    Communication.transfer_file f cstate

let main () =
  let peer = {ip="10.132.6.253"; key="123abc"} in
  (*
  let _ = Communication.request_file peer "test.txt" "recv.txt" >>= fun () ->
    print_string "Success!"; Async.Deferred.return ()
  in
*)
  let _ = Communication.send_state peer "['crap.txt', 'goto.crap', 'charles <3 Jackie']" >>= fun () ->
    print_string "Success!"; Async.Deferred.return ()
  in
  Scheduler.go ()


let _ = main ()
