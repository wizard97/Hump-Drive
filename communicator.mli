open Unix
open Crypto

(* Used to store information about current connection *)
type state

(* Store info about a peer *)
type peer

(* Initial state of the communicator *)
val init : state

(* Discover peers on the local network *)
val discover_peers : state -> peer list

(* Return true if peer reachable *)
val peer_reachable : peer -> bool


val peer_pair : peer -> key -> state
