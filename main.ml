(* Uses communicator to discover other peers *)
(* Establishes a connection via communicator *)
(* If differences on either this machine or the other, transfer handles updates *)
(* GUI displays all stuff *)
(* Crypto encrpyts files and user info/ connection-establishing processes *)
open Filetransfer
open Communication
open Async
open Async.Reader

(*
let main () =
  ANSITerminal.(print_string [green]
                  "\n\nWelcome to Hump Drive. \n");
  loop () *)


(* Empty function for converting deferred to unit *)
let to_unit d = upon d (fun _ -> ())

let notify_callback cstate _ msg =
  match msg with
  | State _ -> print_string "Got state update!"; Async.Deferred.return ()
  | Filerequest f ->
    print_string "Got request for file!";
    Communication.transfer_file f cstate


let server () =
   (* let peer = {ip="127.0.0.1"; key="123abc"} in *)
  print_string "Running Server\n";
  Communication.start_server notify_callback


let client () =
  let peer = {ip = "10.132.7.82"; key="hjga"} in
  Communication.request_file peer "ydf.mp4" "recv.mp4" >>= fun () ->
    print_string "Success!\n"; Async.Deferred.return ()


(* Given an input string from the repl, handle the command *)
let process_input = function
| "about" -> print_endline "*****Version 1.0****"
| "quit" -> print_endline "Done"; upon (exit 0) (fun _ -> ())
| "run server" ->  server () |> to_unit
| "run client" ->  client () |> to_unit
|_ -> print_endline "Invalid Command!"

(* Repl for filesyncing interface *)
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

let _ = repl ()
let _ = Scheduler.go ()
