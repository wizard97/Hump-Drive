(* Scott Dickson (sdd48)
 * Charles Yu (ccy27)
 * Zack Brienza (zb5)
 * Aaron Wisner (daw268)
*)

open String

(* Number base for the string form of the keys *)
type key = string

let key_from_string s = s

let string_from_key s = s

let generate_public_private () = ("key", "key")

let encrypt_line s key = s

let decrypt_line s key1 key2 = s

let compare s1 s2 = s1 = s2
