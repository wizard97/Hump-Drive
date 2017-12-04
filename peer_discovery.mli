
open Async
open Async_extra

val port : int

type listen_state

val broadcast : unit -> unit Import.Deferred.t
val listen : (string -> string -> unit) -> listen_state Import.Deferred.t
