(* Scott Dickson (sdd48)
 * Charles Yu (ccy27)
 * Zack Brienza (zb5)
 * Aaron Wisner (daw268)
*)

open String

(* Number base for the string form of the keys *)
let b = 57
let exp = 17 (* Encryption exponent *)

(* max key size is 11 so find a 5 digit prime *)

(***** HELPERS *****)

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


(* Extended euclidean algorithm. Adapted from the wikibooks
 page on the algorithm *)
let rec egcd a b m=
  if a = 0 then (b,0,1)
  else let (g,y,x) = egcd (b mod a) a m in
  (g,x - (b/a)*y, y)

let modinv a m =
  let (g,x,y) = egcd a m m in
  if g <> 1 then failwith "No inverse"
  else let x' = x mod m in
  if x' < 0 then x' + m else x'


let rec string_from_key' k s =
  if k = 0  && s = "" then "A" else
  if k = 0 then s else
  let r = k mod b in
 string_from_key' ((k - r)/b) (Char.( r+65 |> chr |> escaped)^s)

(* Convert the int key to a readable string *)
let string_from_key k = string_from_key' k ""

(***** END HELPERS *****)

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

(* Returns tuple of pu*pr *)
let generate_public_private ()=
  let x = generate_key () in
  (x*x |> string_from_key ,string_from_key x)

let encrypt_line s k =
  failwith "Unimplemented"

let decrypt_line s pu pr =
  failwith "Unimplemented"

