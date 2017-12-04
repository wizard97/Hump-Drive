
open Async
open Async_extra
open Peer_discovery

let discovered addr msg =
  print_string addr;
  print_string msg

let _ = Peer_discovery.listen discovered
let _ = Scheduler.go ()
