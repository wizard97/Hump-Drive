(* Scott Dickson (sdd48)
 * Charles Yu (ccy27)
 * Zack Brienza (zb5)
 * Aaron Wisner (daw268)
*)


(* The key type is an enormous prime number ht be used
 * in securing message in a fasion similar to the RSA protocol. *)
type key

val chunk_size : int

(* Given a short cypher phrase generate a much larger key
 * treat the string as an integer in base 62 with A-0,B-1, ... ,8-60,9-61 *)
val key_from_string : string -> key

(* Given a large integer that is the key, return a string serving
 * as the compressed format of that key for easy transmission. *)
val string_from_key : key -> string

(* Generates a random public, private key pair to be used with the
 * below encrypt line/decrypt line functions. *)
val generate_public_private : unit -> key * key

(* Given the public key of a target device and a public key,
 * return a string containing the encrypted message.
 * The encrypted message can then be sent to the desired recipient. *)
val encrypt_line : string -> key ->  string

(* Given an encrypted message, the decrypting device's private key
 * and the sending device's public key return the decrypted message *)
val decrypt_line : string -> key -> key -> string

(* Determine whether two keys are equal *)
val compare : key -> key -> bool

(* Given a large string generate the encrypted message padded by the chunk size *)
val encrypt_and_chunk : string -> key -> string

(* Given a string chunked by the above method, decrypt is and return the
 * original message. *)
val decrypt_chunked : string -> key -> key -> string

val test_modinv : int -> unit

(*
val modinv : key -> key -> key

val mod_exp : key -> key -> key -> key

val large_int_to_string : key -> string

val string_to_large_int : string -> key
*)