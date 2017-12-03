
open Async
open Async_extra
open Peer_discovery


let _ = Peer_discovery.listen "127.0.0.1"
let _ = Scheduler.go ()