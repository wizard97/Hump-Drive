(* Uses communicator to discover other peers *)
(* Establishes a connection via communicator *)
(* If differences on either this machine or the other, transfer handles updates *)
(* GUI displays all stuff *)
(* Crypto encrpyts files and user info/ connection-establishing processes *)
open Filetransfer
open Communication
open Async
open Async.Reader

(* let tensec () =
  upon (after (Core.sec 5.)) (fun _ -> print_endline "\n10 seconds have elapsed!\n")

let rec loop () =
  print_string  "> ";
  upon (Reader.read_line (Lazy.force Reader.stdin))
  (fun r ->
    match r with
    |`Eof -> ()
    |`Ok s -> if s = "10sec" then (tensec (); loop ()) else loop ()
  )

let main () =
  ANSITerminal.(print_string [green]
                  "\n\nWelcome to Hump Drive. \n");
  loop () *)

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

let () = main ()
