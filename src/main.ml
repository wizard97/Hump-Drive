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

(* Hashtbl to map public keys to peers*)
module KeyHash = struct
  type t = Crypto.key
  let equal k1 k2 = Crypto.key_equal k1 k2
  let hash k = Crypto.key_hash k
end


module KeyHashtbl = Hashtbl.Make(KeyHash)

(* Helper to exit gracefully from Async*)
let exit_graceful = fun () -> upon (exit 0) (fun _ -> ())


(* Hash s =, used to checksum in UDP discover packet*)
let compute_hash s =
  let hash = ref 0 in
  let update_hash c =
    let h = !hash in
    let h' = ((h lsl 5) - h) + (Char.code c) in
    hash := h' land h'
  in String.iter (update_hash) s; !hash


(* convert a bcast_msg to its string representation for UDP packet*)
let bcastmsg_to_string (m:bcast_msg) : string =
  let data = Marshal.to_string m [] in
  let payload = (string_of_int (compute_hash data), data) in
  Marshal.to_string payload []



(* Convert a received UDP packets contents back into its bcase type*)
let string_bcast_msg s : bcast_msg option =
  let (hash, buf) : (string*string) = Marshal.from_string s 0 in
  let data : bcast_msg = Marshal.from_string buf 0 in
  if (string_of_int (compute_hash buf)) = hash then
    Some data
  else
    None


(* Empty function for converting deferred to unit *)
let to_unit d = upon d (fun _ -> ())

(* Called periodically to notify the other peer of the state (if known)*)
let rec peer_syncer peers (mypeer:Crypto.key) st =
  upon(after (Core.sec bcast_interval) >>= fun () ->
  if KeyHashtbl.mem peers mypeer then
    let _ = print_endline "Sending state update" in
    let (name,pinfo) = KeyHashtbl.find peers mypeer in
    let _ = State.update_state !st >>= fun ns -> st := ns; Deferred.return () in
    let strs = State.to_string !st in
    let _ = Config.save_st_string strs (State.root_dir !st) in
    Communication.send_state pinfo strs
  else Deferred.return ()) (fun () -> peer_syncer peers mypeer st)


(* Callback function when a peer is dicovered via UDP broadcast *)
let peer_discovered peers addr msg  =
  match (string_bcast_msg msg) with
  | Some (name, key) ->
    KeyHashtbl.add peers key (name,{ip=addr; key=key});
    print_endline ("Found my peer: "^name^" "^addr)
  | None -> print_string "Garbage!"


(* Upon receiving a state update, request files from that peer if neccisary*)
let proc_state_update pubpriv currstate rs pr :state_info Deferred.t  =
  let ups = State.files_to_request currstate rs in
  print_endline (string_of_int (List.length ups)^" files");
  let recf st f = (Communication.request_file pubpriv pr f
                     ((State.root_dir currstate)^Filename.dir_sep^f))
                                              >>= fun () -> st >>= fun st' ->
    print_endline ("Recvd file :"^f);
    (State.acknowledge_file_recpt st' f)
  in
  List.fold_left recf (Deferred.return currstate) ups



(* The listening TCP server for incoming requests.
 * Upon receiving a request, will decrypt message and handle request
*)
let comm_server pubpriv currstate lockstate rset mypeer =
  let notify_callback cstate pr msg =
    match msg with
    | State s ->
      begin
        print_endline "Received state update from peer!";
        let rst = State.from_string s in
        match !rset with
        | None when (not !lockstate)->
          lockstate := true;
          rset := Some rst; proc_state_update pubpriv
                                        (!currstate) rst pr >>= fun ns ->
          let _  = Config.save_state ns (State.root_dir ns) in
          currstate := ns;
          lockstate := false;
          Deferred.return (rset := None)
        | _ -> Deferred.return () (* Ignore if already being processed*)
      end
    | Filerequest f when (not !lockstate) ->
      print_endline ("Incoming file request for: "^f);
      lockstate := true;
      Communication.transfer_file mypeer
        ((State.root_dir !currstate)^Filename.dir_sep^f) cstate >>= fun () ->
      Deferred.return (lockstate := false)
    | _ -> Deferred.return (())
  in
  print_endline "Starting incoming request server";
  Communication.start_server notify_callback mypeer



(* Called periodically to send a discovery broadcast UDP packet onto network *)
let rec peer_broadcaster msg =
  upon(after (Core.sec bcast_interval) >>= fun () ->
                          Peer_discovery.broadcast msg )
    (fun () -> print_endline "Sending discover packet"; peer_broadcaster msg)


(* [rdir] is the path to the root directory, calling this function will
 * try and load the public and private keeys from the config directory
 * If none exist, they will be created and saved
*)
let load_keys rdir =
  print_endline "Looking for keys...";
  try
    Config.load_pubkey rdir >>= fun pub ->
    Config.load_privkey rdir >>= fun priv ->
    Deferred.return (Crypto.key_from_string pub, Crypto.key_from_string priv)
  with exn ->
    let _ = print_string "Creating new keys.\n" in
    let (pub, priv) = Crypto.generate_public_private () in
    let (pubs, privs) = (Crypto.string_from_key pub), (Crypto.string_from_key priv) in
    Config.write_file pubs Config.fname_PUBKEY rdir >>= fun () ->
    Config.write_file privs Config.fname_PRIVKEY rdir >>= fun () ->
    Async.return (pub,priv)


(* [rdir] is the path to the root directory, calling this function will
 * try and load the peer public key, upon failure, the program will exit with a message
*)
let load_peerkey rdir =
  print_endline "Looking for peer keys...";
  try
    Config.load_peerkey rdir >>= fun pk ->
    Deferred.return (Crypto.of_string pk)
  with exn ->
    print_endline "PLEASE ADD YOUR PEERS PUBLIC KEY TO peerkey!";
    exit_graceful ();
    Deferred.return (Crypto.of_string "0")


(* Initializes all servers and returns the ref of the current state. *)

let launch_synch rdir =
  load_keys rdir >>= fun (pub,priv) -> load_peerkey rdir >>= fun peerkey ->
  let _ = print_endline "Scanning directory..." in
  let st =
    print_endline "Looking for saved states...";
    try
      Config.load_state rdir
    with exn ->
      print_string
        "Could not find a saved state. Either no saved state or corrupt...\nEstablishing new state.\n";
      State.state_for_dir rdir
  in
  print_endline "State successfully loaded!";
  st >>= fun sinfo ->
  let _ = print_endline "Starting comm server" in
  let rstate = ref None in
  let lockstate = ref false in
  State.update_state sinfo >>= fun cs ->
  let currstate = ref cs in
  let discovered_peers = KeyHashtbl.create 5 in
  comm_server (pub,priv) currstate lockstate rstate peerkey >>= fun _ ->
  print_endline "Starting discovery broadcaster";
  peer_broadcaster (bcastmsg_to_string ("Computer A", pub));
  print_endline "Starting discovery server";
  let _ = Peer_discovery.listen (peer_discovered discovered_peers) in
  let _ = peer_syncer discovered_peers peerkey currstate in
  Config.save_state sinfo rdir >>= fun _ ->
  Deferred.return (print_endline "Init complete!")


(* Given an input string from the repl, handle the command *)
let process_input = function
| "about" -> print_endline "*****Version 1.0****"
| "quit" | "exit" -> print_endline "Exiting gracefully..."; exit_graceful ()
| "help" -> print_endline "Stuck? Type <quit> or <exit> at any point to exit gracefully."
|_ -> print_endline "Invalid Command!"


(* Async loop to evaluate input commands from stdin when availible *)
let rec loop () =
  print_string ">>> ";
  (Reader.stdin |> Lazy.force |> Reader.read_line |> upon)
    begin fun r ->
      match r with
      | `Ok s -> process_input s; loop ()
      | `Eof ->  print_endline "What happened"; exit_graceful ()
    end


(* Read directory path from REPL, when prompted for selection of root directory*)
let get_dir_path () =
  print_endline "Please type in the directory path you wish to sync.";
  (Reader.stdin |> Lazy.force |> Reader.read_line) >>= fun r ->
    match r with
    | `Ok s -> begin
      try let p = (Config.path_ok s) in Deferred.return p
      with exn -> print_endline "That is not a valid path!";
        exit_graceful(); Deferred.return ("Oops")
    end
    | `Eof ->  exit_graceful(); Deferred.return("Oops")

(* Repl for filesyncing interface *)
let repl () =
  let _ = print_string "\n\nWelcome to Hump-Drive Version 1.0!\nMake sure you have configured everything correctly.\nConsult the report for configuration details if needed.\nType <start> to begin. Type <quit> or <exit> at any point to exit gracefully.\n\n";
  print_string " >>> " in
  (Reader.stdin |> Lazy.force |> Reader.read_line) >>= fun r ->
    match r with
    | `Ok s ->
      if s = "start" then
        (get_dir_path ()) >>= fun dpath ->
        (launch_synch dpath) >>= fun _ ->
        loop(); Deferred.return ()
      else Deferred.return (exit_graceful ())
    | `Eof -> Deferred.return (exit_graceful ())


(* Launch all the Async stuff *)
let main () =
  let _ = repl () in
  Scheduler.go ()

let _ = main ()
