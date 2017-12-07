(* Scott Dickson (sdd48)
 * Charles Yu (ccy27)
 * Zack Brienza (zb5)
 * Aaron Wisner (daw268)
*)


(* The key type is an enormous prime number to be used
 * in securing message in a fasion similar to the RSA protocol. *)
type key

(* Size of each the chunks to be read from files to be sent  *)
val chunk_size : int

(* Size of the chunks to be read from a sent file transmission
 * to be decrypted and the stored *)
val output_chunk_size : int

(* true if key1 ~ key2 *)
val key_equal : key -> key -> bool

(* Given a key return a hash *)
val key_hash : key -> int

(* Works as expected *)
val of_string : string -> key


(* Given a short cypher phrase of ascii characters,
 * treat the string as an integer in base 256 and return the value
 * of the string under that interpretation. *)
val key_from_string : string -> key

(* Given a key, return a string serving representing its base 256 value
 * for transmission across the network.
 *)
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

(* Given a large string generate the encrypted message padded by the chunk size *)
val encrypt_and_chunk : string -> key -> string

(* Given a string chunked by the above method, decrypt is and return the
 * original message. *)
val decrypt_chunked : string -> key -> key -> string


