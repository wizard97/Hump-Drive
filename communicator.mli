open Unix
open Crypto

(* This module will use UDP to communicate with other peers *)

(* Used to store information about current connection *)
type state

(* Store info about a peer *)
type peer

(* Events you can send to notify peer *)
type event

(* Initial state of the communicator *)
val init : state

(* Discover peers on the local network *)
val discover_peers : state -> peer list

(* Return true if peer reachable *)
val peer_reachable : state -> peer -> bool

(* Pair with a peer who has key [key] *)
val peer_pair : state -> peer -> Crypto.key -> state


(* Notfy the peer of an event such as an updated file *)
val notify_peer : state -> peer -> event -> Crypto.key -> state


(* periodically call this to notify your peer of state of files*)
val notify_state : state -> peer -> Crpyto.key -> state
