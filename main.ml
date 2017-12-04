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


(* Given an input string from the repl, handle the command *)
let process_input = function
| "about" -> print_endline "*****Version 1.0****"
| "quit" -> print_endline "Done"; upon (exit 0) (fun _ -> ())
|_ -> print_endline "Invalid Command!"



(* Repl for *)
let repl () =
  (* let reader = Reader.stdin |> Lazy.force in *)
  let rec loop () =
    print_string " >>> ";
    (Reader.stdin |> Lazy.force |> Reader.read_line |> upon) begin fun r ->
      match r with
      | `Ok s -> process_input s; loop ()
      | `Eof -> print_endline "What happened"
      ;
    end
  in loop ()


let main () =
  let peer = {ip="127.0.0.1"; key="123abc"} in
  print_string "Testing123";
  Communication.start_server notify_callback
  let _ = Scheduler.go ()

(* let () = main () *)

let _ = repl ()
let _ = Scheduler.go ()
