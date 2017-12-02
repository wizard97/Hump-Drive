(* Scott Dickson (sdd48)
 * Charles Yu (ccy27)
 * Zack Brienza (zb5)
 * Aaron Wisner (daw268)
*)

type key
(*type cypher_state *)


(* Given a short cypher phrase generate a much larger key
 * treat the string as an integer in base 62 with A-0,B-1, ... ,8-60,9-61 *)
val key_from_string : string -> key

(* Given a key produce a state that represented the cypher
 * state before any chunks of the message were encrypted *)
(*val init_cypher : key -> cypher_state *)
val init_cypher : key -> unit

(* Genrate a random, large prime number to be used as a key *)
val generate_public_private : unit -> key * key

(* Given the public key of a target device and a packey
 * returns a string representation of the encrypted string
 * this will be used to send updates to each affected device *)
val encrypt_line : string -> key ->  string
(* Or to string*cypher_state*)

(* Given an encrypted message, the decrypting device's private key
 * and the sending device's public key *)
val decrypt_line : string -> key -> key -> string

