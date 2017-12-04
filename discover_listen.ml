
open Async
open Async_extra
open Peer_discovery


let _ = Peer_discovery.listen "192.168.1.255"
let _ = Scheduler.go ()
