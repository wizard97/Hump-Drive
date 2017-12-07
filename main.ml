(* Uses communicator to discover other peers *)
(* Establishes a connection via communicator *)
(* If differences on either this machine or the other, transfer handles updates *)
(* GUI displays all stuff *)
(* Crypto encrpyts files and user info/ connection-establishing processes *)
open Communication
open Crypto
open State
open Async
open Async.Reader
open Async_extra
open Peer_discovery
open Config

let bcast_interval = 5.

(* Name, pubkey*)
type disc_peer = string*Communication.peer
type bcast_msg = string*Crypto.key


let compute_hash s =
  let hash = ref 0 in
  let update_hash c =
    let h = !hash in
    let h' = ((h lsl 5) - h) + (Char.code c) in
    hash := h' land h'
  in String.iter (update_hash) s; !hash


let bcastmsg_to_string (m:bcast_msg) : string =
  let data = Marshal.to_string m [] in
  let payload = (string_of_int (compute_hash data), data) in
  Marshal.to_string payload []


let string_bcast_msg s : bcast_msg option =
  let (hash, buf) : (string*string) = Marshal.from_string s 0 in
  let data : bcast_msg = Marshal.from_string buf 0 in
  if (string_of_int (compute_hash buf)) = hash then
    Some data
  else
    None


(* Empty function for converting deferred to unit *)
let to_unit d = upon d (fun _ -> ())


let rec peer_syncer peers (mypeer:Crypto.key) st =
  upon(after (Core.sec bcast_interval) >>= fun () ->
       print_string "In peer syncer";
  if Hashtbl.mem peers mypeer then
    let _ = print_endline "Attempting to sync" in
    let (name,pinfo) = Hashtbl.find peers mypeer in
    let _ = State.update_state !st >>= fun ns -> st := ns; Deferred.return () in
    let strs = State.to_string !st in
    print_string "Send: "; print_int (compute_hash strs); print_endline "";
    Communication.send_state pinfo strs
  else Deferred.return (print_endline "No peer found")) (fun () -> peer_syncer peers mypeer st)


let peer_discovered (peers: ((Crypto.key, disc_peer) Hashtbl.t)) addr msg  =
  print_string "Peer discovered";
  match (string_bcast_msg msg) with
  | Some (name, key) ->
    Hashtbl.add peers key (name,{ip=addr; key=key});
    print_endline ("Found peer: "^name^" "^addr^": "^(Crypto.string_from_key key))
  | None -> print_string "Garbage!"


let proc_state_update currstate rs pr :state_info Deferred.t  =
  let ups = State.files_to_request currstate rs in
  print_endline (string_of_int (List.length ups)^" files");
  let recf st f :state_info Deferred.t = (Communication.request_file pr f ((State.root_dir currstate)^f)) >>= fun () -> st >>= fun st' ->
    print_endline ("Recvd file:"^f);
    (State.acknowledge_file_recpt st' f)
  in
  List.fold_left recf (Deferred.return currstate) ups


let comm_server currstate rset mypeer = (* TODO make sure peer is who we think it is*)
  let notify_callback cstate pr msg =
    match msg with
    | State s ->
      print_string "Got: "; print_int (compute_hash s); print_endline "";
      begin
        print_string "Got state update!";
        let rst = State.from_string s in
        match !rset with
        | None -> rset := Some rst; proc_state_update (!currstate) rst pr >>= fun ns ->
          currstate := ns;
          Deferred.return (rset := None)
        | _ -> Deferred.return (print_endline "Pending update!") (* Ignore if already being processed*)
      end
    | Filerequest f ->
      print_string "Got request for file!";
      Communication.transfer_file ((State.root_dir !currstate)^f) cstate
  in
  print_string "Running Server\n";
  Communication.start_server notify_callback


let rec peer_broadcaster msg =
  upon(after (Core.sec bcast_interval) >>= fun () ->
                          Peer_discovery.broadcast msg )
    (fun () -> print_endline "sent bcast"; peer_broadcaster msg)

(* Initializes all servers and returns the ref of the current state. *)
let launch_synch () =
  let rdir = "test/" in
  let mypeer = Crypto.key_from_string "peer2" in (* TODO fix this*)
  let mypub = Crypto.key_from_string "peer1" in (* TODO fix this*)
  let _ = print_endline "Scanning directory... \n" in
  let st =
    print_endline "Looking for saved states...";
    try
      Config.load_state rdir
    with exn ->
      print_string "Could not find a saved state. Either no saved state or corrupt...\nEstablishing new state.\n";
      State.state_for_dir rdir
  in
  print_endline "State successfully loaded!";
  st >>= fun sinfo ->
  let _ = print_endline "Starting comm server" in
  let rstate = ref None in
  let currstate = ref sinfo in
  let discovered_peers : ((Crypto.key, disc_peer) Hashtbl.t) = Hashtbl.create 5 in
  comm_server currstate rstate mypeer >>= fun _ ->
  print_endline "Starting discovery broadcaster";
  peer_broadcaster (bcastmsg_to_string ("Computer A", mypub));
  print_endline "Starting discovery server";
  let _ = Peer_discovery.listen (peer_discovered discovered_peers) in
  let _ = peer_syncer discovered_peers mypeer currstate in
  Config.save_state sinfo rdir >>= fun _ -> (print_endline "Init complete!");
    Deferred.return (rstate)


(* Given an input string from the repl, handle the command *)
let process_input = function
| "about" -> print_endline "*****Version 1.0****"
| "quit" -> print_endline "Exiting gracefully..."; upon (exit 0) (fun _ -> ())
|_ -> print_endline "Invalid Command!"


(* Repl for filesyncing interface *)
let repl st_ref =

  let rec loop () =
    print_string " >>> ";
    (Reader.stdin |> Lazy.force |> Reader.read_line |> upon)
    begin fun r ->
      match r with
      | `Ok s -> process_input s; loop ()
      | `Eof -> print_endline "What happened"
    end
  in loop ()


let main () =
  let _ = launch_synch () |> repl in
  Scheduler.go ()

let _ = main ()
