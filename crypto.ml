(* Scott Dickson (sdd48)
 * Charles Yu (ccy27)
 * Zack Brienza (zb5)
 * Aaron Wisner (daw268)
*)

open String

(* Number base for the string form of the keys *)
type key = Big_int.big_int


let zero = Big_int.zero_big_int
let bMod = Big_int.mod_big_int
let eq = Big_int.eq_big_int
let add = Big_int.add_big_int
let add_i = Big_int.add_int_big_int
let sub = Big_int.sub_big_int
let mult = Big_int.mult_big_int
let mult_i = Big_int.mult_int_big_int
let div = Big_int.div_big_int
let half b = div b (Big_int.big_int_of_int 2)
let decr b = add b (Big_int.big_int_of_int (-1))
let even b = eq (bMod b (Big_int.big_int_of_int 2)) zero
let of_int = Big_int.big_int_of_int
let to_int = Big_int.int_of_big_int

let b = of_int 43
let exp = of_int 17 (* Encryption exponent *)




(* max key size is 11 so find a 5 digit prime *)

(***** HELPERS *****)

let last_char s =
  get s (length s - 1)

let strip_last s =
  String.sub s 0 (String.length s - 1)


(* RequiresL ints a,b, and m *)
(* Returns: a^b mod m while avoiding overflow *)
let rec mod_exp a b m =
  if eq b zero then of_int 1 else
  if even b then
    let v = add (mod_exp a (half b) m) m in
    let x = bMod v m in bMod (mult x x) m
  else
    let v = add (mod_exp a (decr b) m) m in
    bMod (mult (bMod a m)  (bMod v m)) m


(* Requires: int n*)
(* Returns: whether n is prime. Based on the fermat test *)
let is_prime n =
  let x1 = of_int 17 in (* {1,..,n-1} *)
  let x2 = of_int 31 in
  eq (mod_exp x1 (decr n) n) (of_int 1) &&
  eq (mod_exp x2 (decr n) n) (of_int 1)


(* Extended euclidean algorithm. Adapted from the wikibooks
 page on the algorithm *)
let rec egcd a b m=
  if eq a zero then (b, zero, of_int 1)
  else let (g,y,x) = egcd (bMod b a) a m in
  (g,sub x (mult (div b a) y), y)

let modinv a m =
  let (g,x,y) = egcd a m m in
  if not (eq g (of_int 1)) then failwith "No inverse"
  else let x' = bMod x m in
  if Big_int.lt_big_int x' zero
  then Big_int.add_big_int x' m
  else x'


(* Given a string of any ascii characters return its
 * big int representation *)
let rec string_to_large_int s =
  if s = "" then zero else
  add_i (last_char s |> Char.code)
    (mult_i 256 (string_to_large_int (strip_last s)) )



(* Inverse of above *)

let rec large_int_to_string' n s=
  if eq n zero then s else
  let r = bMod n (of_int 256) in
  large_int_to_string'
  (div (sub n r) (of_int 256)) ((to_int r |> Char.chr |> Char.escaped)^s)

let large_int_to_string n = large_int_to_string' n ""



let rec string_from_key' k s =
  if eq k zero  && s = "" then "" else
  if eq k zero then s else
  let r = bMod k b in
 string_from_key' (div (sub k r) b) (Char.( (to_int r)+48 |> chr |> escaped)^s)

(* Convert the int key to a readable string *)
let string_from_key k = string_from_key' k ""


let rec key_from_string s =
  if length s = 0 then zero
  else
(*   let str = "Digit is "^(string_of_int (Char.code (last_char s) - 48)) in
   print_endline str; *)
let s' = (String.sub s 0 (length s - 1)) in
add_i (Char.code (last_char s) - 48) (mult b (key_from_string s'))


(***** END HELPERS *****)

let init_cypher k =
  failwith "Unimplemented"

let rec big_random' acc n =
  if n = 0 then Big_int.big_int_of_string acc
  else big_random' ((Random.int 1000000000 |> string_of_int)^acc) (n-1)


(* Generate a random very large integer *)
let big_random () =
  big_random' "" 20



(* Ranges from 0 to [612436557] in decimal or
  A to zzzzz in our char base 57. Square this number to make a public/private*)
let generate_key =
  Random.self_init ();
  let rec loop () =
    let p = big_random () in
    if is_prime p then p else loop ()
  in loop

(* Returns tuple of pu*pr *)
let generate_public_private ()=
  let x = generate_key () in
  (mult x x ,x)


(* Convert to int, encrypt, convert back to string
let encrypt_string k s =
  mod_exp (string_to_large_int s) exp k |> large_int_to_string *)

let encrypt_string k s =
  mod_exp (key_from_string s) exp k |> string_from_key




let rec chunk' s acc n=
  if s = "" then acc else
  if String.length s <= n then List.rev (s::acc)  else
  let all_but_n str = String.sub str n (String.length str - n) in

  chunk' (all_but_n s) ((String.sub s 0 n)::acc) n

(* Given a string return a list of the chunks *)
let chunk s = chunk' s [] 5




let encrypt_line s pu =
    encrypt_string pu s

(*
    let lst = List.map (encrypt_string pu) (chunk s) in
     List.fold_right (fun e acc -> e^acc) lst ""
*)

(*
let decrypt_line s pu pr =
  let k' = modinv exp (mult pr (decr pr) ) in
  let s' = string_to_large_int s in
  mod_exp s' k' pu |> large_int_to_string *)


let decrypt_line s pu pr =
  let k' = modinv exp (mult pr (decr pr) ) in
  let s' = key_from_string s in
  mod_exp s' k' pu |> string_from_key
