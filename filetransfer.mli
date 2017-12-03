open Async
open Async.Tcp_file
open Async_extra

type 'a result = FT_Error of string | FT_Success of 'a


val port : int

type ftclient =
  (Tcp_file.Client.t, Base.Exn.t) Core.Result.t Async_extra.Import.Deferred.t

(* Server stuff *)
val create_server : unit -> Tcp.Server.inet Import.Deferred.t

val server_add_file : string -> unit Import.Deferred.t


(* Client Stuff *)
val client_connect :
  string -> ftclient

val client_get_file : ftclient -> string -> string -> ((unit result) Async_extra.Import.Deferred.t)
