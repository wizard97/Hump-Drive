open Async
open Async_extra.Tcp
open Async_extra.Import.Reader

type message = State of string | Filerequest of string

(* ip, key *)
type peer = { ip:string; key:string;} (* Todo make crypto key *)


val port : int


val start_server : (message -> unit) -> (Socket.Address.Inet.t, int) Server.t Deferred.t


val request_file : peer -> string -> (unit -> unit) -> unit Async.Deferred.t


val send_state : peer -> string -> (unit -> unit) -> unit Async.Deferred.t
