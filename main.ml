(* Uses communicator to discover other peers *)
(* Establishes a connection via communicator *)
(* If differences on either this machine or the other, transfer handles updates *)
(* GUI displays all stuff *)
(* Crypto encrpyts files and user info/ connection-establishing processes *)
open Communication
open Crypto
open Database
open Async
open Async.Reader
open Async_extra
open Peer_discovery

let bcast_interval = 5.

(* Name, pubkey*)
type bcast_msg = string*Crypto.key

let compute_hash s =
  let hash = ref 0 in
  let update_hash c =
    let h = !hash in
    let h' = ((h lsl 5) - h) + (Char.code c) in
    hash := h' land h'
  in String.iter (update_hash) s; !hash


let bcastmsg_to_string m : string =
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



let peer_discovered mypeer foundpeer myst addr msg  =
  match (string_bcast_msg msg) with
  | Some (name, pubkey) when not !foundpeer->
    if (mypeer = pubkey) then
      let _ = print_endline ("Found peer: "^name^" "^addr) in
      let stpick = Database.to_string myst in
      let pr : peer = {ip=addr; key=pubkey} in
      foundpeer := true;
      let _ = Communication.send_state pr stpick in
      foundpeer := false
    else
      print_endline ("Found different peer: "^name^": "^addr)
  | Some _ -> ()
  | None -> print_string "Garbage!"



let proc_state_update currstate rs pr :state_info Deferred.t  =
  Database.update_state currstate >>= fun nstate ->
  let ups = Database.files_to_request nstate rs in
  let recf st f :state_info Deferred.t = (Communication.request_file pr f f) >>= fun () ->
    Database.acknowledge_file_recpt st f
  in
  List.fold_left recf (Deferred.return nstate) ups


let comm_server currstate rset mypeer = (* TODO make sure peer is who we think it is*)
  let notify_callback cstate pr msg =
    match msg with
    | State s ->
      begin
        print_string "Got state update!";
        let rst = Database.from_string s in
        match !rset with
        | None -> rset := Some rst; proc_state_update (!currstate) rst pr >>= fun ns ->
          currstate := ns;
          Deferred.return (rset := None)
        | _ -> Deferred.return ()
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
  let mypeer = Crypto.key_from_string "" in (* TODO fix this*)
  let mypub = Crypto.key_from_string "" in (* TODO fix this*)
  let _ = print_endline "Scanning directory" in
  Database.state_for_dir "submission/" >>= fun sinfo ->
  let _ = print_endline "Starting comm server" in
  let rstate = ref (Some sinfo) in
  let currstate = ref sinfo in
  comm_server currstate rstate mypeer >>= fun _ ->
  print_endline "Starting discovery broadcaster";
  peer_broadcaster (bcastmsg_to_string ("Computer A", mypub));
  print_endline "Starting discovery server";
  let foundpeer = ref false in
  let _ = Peer_discovery.listen (peer_discovered mypeer foundpeer sinfo) in
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
