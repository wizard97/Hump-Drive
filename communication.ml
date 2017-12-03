open Async
open Async_extra.Tcp
open Async_extra.Import.Reader

type message = State of string | Filerequest of string

(* ip, key *)
type peer = { ip:string; key:string;} (* Todo make crypto key *)

let port = 31100


let cmp_sub s cmd =
  try (
    let ss = String.sub s 0 (String.length s) in
    ss = cmd
  ) with
  _ -> false


let msg_to_string = function
  | State s -> "state:"^s^"\n"
  | Filerequest s -> "freq:"^s^"\n"


let process_cmd s hookup =
  let scmp = cmp_sub s in
  let slen = String.length s in
  if (scmp "state:") then
    let ssl = String.length "state:" in
    hookup (State (String.sub s ssl (slen-ssl)));
    Deferred.return ()
  else if (scmp "freq:") then
    let ssl = String.length "freq:" in
    hookup (Filerequest (String.sub s ssl (slen-ssl)));
    Deferred.return ()
  else
    Deferred.return ()


let start_server hookup =
  let server_callback addr read _ =
    print_string "Got Client!";
    let saddr = Socket.Address.Inet.addr addr |> Unix.Inet_addr.to_string in
    saddr |> print_string;
    read_until read (`Char '\n') ~keep_delim:(false) >>= fun r ->
    match (r) with
    | `Ok s -> process_cmd s (hookup {ip=saddr; key="NULL"})
    | `Eof_without_delim s ->
      print_endline ("Invalid command: "^s); Deferred.return ()
    | `Eof -> Deferred.return ()
      >>= fun () ->
      print_string "Closed connection!";
      close read
  in
  Server.create (on_port port) server_callback



let send_message peer msg onsucc =
  connect (to_host_and_port peer.ip port) >>= fun (sock, _, writer) ->
  print_string "Connected to server to send notification!";
  let smesg = msg_to_string msg in
  Writer.write writer smesg;
  Writer.flushed writer >>= fun () ->
  Writer.close writer >>= fun () ->
  onsucc ();
  Deferred.return ()



let request_file peer fname onsucc =
  send_message peer (Filerequest fname) onsucc


let send_state peer state onsucc =
  send_message peer (State state) onsucc
