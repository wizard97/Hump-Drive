(* Test the peer discovery functionality *)

open Async
open Async_extra
open Peer_discovery

let _ = Peer_discovery.broadcast "Hello world"
let _ = Scheduler.go ()
