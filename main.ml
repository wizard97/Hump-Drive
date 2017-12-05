(* Uses communicator to discover other peers *)
(* Establishes a connection via communicator *)
(* If differences on either this machine or the other, transfer handles updates *)
(* GUI displays all stuff *)
(* Crypto encrpyts files and user info/ connection-establishing processes *)
open Communication
open Async
open Async.Reader
open Async_extra
open Peer_discovery

let bcast_interval = 5.

(* Name, pubkey*)
type bcast_msg = string*int

let compute_hash s =
  let hash = ref 0 in
  let update_hash c =
    let h = !hash in
    let h' = ((h lsl 5) - h) + (Char.code c) in
    hash := h' land h'
  in String.iter (update_hash) s; !hash


let bcastmsg_to_string m =
  let data = Marshal.to_string m [] in
  let payload = (string_of_int (compute_hash data), data) in
  Marshal.to_string payload []


let string_bcast_msg s =
  let (hash, buf) : (string*string) = Marshal.from_string s 0 in
  let data : bcast_msg = Marshal.from_string buf 0 in
  if (string_of_int (compute_hash buf)) = hash then
    Some data
  else
    None



(* Empty function for converting deferred to unit *)
let to_unit d = upon d (fun _ -> ())



let peer_discovered addr msg =
  match (string_bcast_msg msg) with
  | Some (name, pubkey) -> print_endline name
  | None -> print_string "Garbage!"



let comm_server () =
  let notify_callback cstate _ msg =
    match msg with
    | State _ -> print_string "Got state update!"; Async.Deferred.return ()
    | Filerequest f ->
      print_string "Got request for file!";
      Communication.transfer_file f cstate
  in
  print_string "Running Server\n";
  Communication.start_server notify_callback


let rec peer_broadcaster msg =
  upon(after (Core.sec bcast_interval) >>= fun () ->
                          Peer_discovery.broadcast msg )
    (fun () -> print_endline "sent bcast"; peer_broadcaster msg)


let launch_synch () =
  print_endline "Scanning directory";
  (* TODO connect into directory scanner*)
  print_endline "Starting comm server";
  comm_server () >>= fun _ ->
  print_endline "Starting discovery broadcaster";
  peer_broadcaster (bcastmsg_to_string ("Computer A", 1234567));
  print_endline "Starting discovery server";
  let _ = Peer_discovery.listen peer_discovered in
  Deferred.return (print_string "Init complete")


let client () =
  let peer = {ip = "10.132.7.82"; key="hjga"} in
  Communication.request_file peer "ydf.mp4" "recv.mp4" >>= fun () ->
    print_string "Success!\n"; Async.Deferred.return ()


(* Given an input string from the repl, handle the command *)
let process_input = function
| "about" -> print_endline "*****Version 1.0****"
| "quit" -> print_endline "Done"; upon (exit 0) (fun _ -> ())
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


let main () =
  let _ = launch_synch () in
  let _ = repl () in
  Scheduler.go ()

let _ = main ()
