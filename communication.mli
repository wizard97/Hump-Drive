open Async
open Async_extra.Tcp
open Async_extra.Import.Reader
open Crypto


type message = State of string | Filerequest of string

(* ip, key *)
type peer = { ip:string; key:Crypto.key;} (* Todo make crypto key *)

type server

type conn_state


val port : int

(*
* [start_server cfb key] will call cfb every time someone attempts to connect
* the callback can either serve the request or shutdown the socket, if they are
* not authenticated
*)
val start_server : (conn_state -> peer -> message -> unit Deferred.t) -> Crypto.key -> server Async.Deferred.t



(*
* [request_file p fname fdest] will request p for file [fname]
* If the peer accepts the request, it will be transferred encrypted over TCP
* into the local file path [fdest]
*)
val request_file : (Crypto.key*Crypto.key) -> peer -> string -> string -> unit Async.Deferred.t


(*
*  [send_state p s] send the current state of the local state of the peer, picked
* into string [s] to the peer on the receiving side
*)
val send_state : peer -> string -> unit Async.Deferred.t


(*
*  Transfer file should only be called in an callback function that is the argument to
* start_server, calling this method will accept the incoming file request
* and it will be served to the client who requested the file with [request_file]
*)
val transfer_file : Crypto.key -> string -> conn_state -> unit Async.Deferred.t
