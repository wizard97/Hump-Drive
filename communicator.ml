

type state = ()
type peer = ()
type event = ()

let init = failwith "U"

let discover_peers s = failwith "U"

(* Return true if peer reachable *)
let peer_reachable s p = failwith "U"

(* Pair with a peer who has key [key] *)
let peer_pair s p k = failwith "U"


(* Notfy the peer of an event such as an updated file *)
let notify_peer s p e k = failwith "U"


(* periodically call this to notify your peer of state of files*)
let notify_state s p k = failwith "U"