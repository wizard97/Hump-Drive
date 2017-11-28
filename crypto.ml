(* Scott Dickson (sdd48)
 * Charles Yu (ccy27)
 * Zack Brienza (zb5)
 * Aaron Wisner (daw268)
*)

open String

(* Number base for the string form of the keys *)
let b = 57
(* max key size is 11 so find a 5 digit prime *)

(* Various helper functions *)

let last_char s =
  get s (length s - 1)

(* RequiresL ints a,b, and m *)
(* Returns: a^b mod m while avoiding overflow *)
let rec mod_exp a b m =
  if b = 0 then 1 else
  if b mod 2 = 0 then
    let x = (mod_exp a (b /2) m) mod m in x*x mod m
  else
    ( (a mod m) * (mod_exp a (b-1) m) ) mod m

(* Requires: int n*)
(* Returns: whether n is prime. Based on the fermat test *)
let is_prime n =
  let x1 = Random.int (n-1) + 1 in (* {1,..,n-1} *)
  let x2 = Random.int (n-1) + 1 in
  mod_exp x1 (n-1) n = 1 &&
  mod_exp x2 (n-1) n = 1

let rec key_from_string s =
  if length s = 0 then 0
  else (Char.code (last_char s) - 65)
    + b * key_from_string (sub s 0 (length s - 1))

let init_cypher k =
  failwith "Unimplemented"

(* Ranges from 0 to [612436557] in decimal or
  A to zzzzz in our char base 57. Square this number to make a public/private*)
let generate_key =
  Random.self_init ();
  let rec loop () =
    let p = Random.int 612436557 in
    if is_prime p then p else loop ()
  in loop

let encrypt_line s k =
  failwith "Unimplemented"

let decrypt_line s pu pr =
  failwith "Unimplemented"

