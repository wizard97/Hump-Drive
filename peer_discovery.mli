
open Async
open Async_extra

val port : int

type listen_state

(* Send over all interfaces' broadcast address a UDP packet
 * containing s
*)
val broadcast : string -> unit Import.Deferred.t


(*
* Upon a recvd incoming broadcast (seend above), [listen f] will call f
* with [f addr data], where addr is the ipv4 address, and data is the original
* data string that was sent
*)
val listen : (string -> string -> unit) -> listen_state Import.Deferred.t
