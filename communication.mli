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


val start_server : (conn_state -> peer -> message -> unit Deferred.t) -> server Async.Deferred.t


val request_file : peer -> string -> string -> unit Async.Deferred.t


val send_state : peer -> string -> unit Async.Deferred.t


val transfer_file : string -> conn_state -> unit Async.Deferred.t
