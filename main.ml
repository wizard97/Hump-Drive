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

let bcast_interval = 5.


module DiscoveredPeers = struct
  type t = string*Communication.peer

  let compare (_,p1) (_,p2)  =
    Pervasives.compare p1.key p2.key
end

module PeerSet = Set.Make(DiscoveredPeers)

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
    let strs = State.to_string !st in
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
  State.update_state currstate >>= fun nstate ->
  let ups = State.files_to_request nstate rs in
  print_endline (string_of_int (List.length ups)^" files");
  let recf st f :state_info Deferred.t = (Communication.request_file pr f f) >>= fun () -> st >>= fun st' ->
    print_endline ("Recvd file:"^f);
    (State.acknowledge_file_recpt st' f)
  in
  List.fold_left recf (Deferred.return nstate) ups


let comm_server currstate rset mypeer = (* TODO make sure peer is who we think it is*)
  let notify_callback cstate pr msg =
    match msg with
    | State s ->
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
      Communication.transfer_file f cstate
  in
  print_string "Running Server\n";
  Communication.start_server notify_callback


let rec peer_broadcaster msg =
  upon(after (Core.sec bcast_interval) >>= fun () ->
                          Peer_discovery.broadcast msg )
    (fun () -> print_endline "sent bcast"; peer_broadcaster msg)


let launch_synch () =
  let mypeer = Crypto.key_from_string "peer1" in (* TODO fix this*)
  let mypub = Crypto.key_from_string "peer2" in (* TODO fix this*)
  let _ = print_endline "Scanning directory" in
  State.state_for_dir "test/" >>= fun sinfo ->
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
  Deferred.return (print_string "Init complete")

    (*
let client () =
  let peer = {ip = "10.132.7.82"; key="hjga"} in
  Communication.request_file peer "ydf.mp4" "recv.mp4" >>= fun () ->
    print_string "Success!\n"; Async.Deferred.return ()
*)

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
