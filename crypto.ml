(* Scott Dickson (sdd48)
 * Charles Yu (ccy27)
 * Zack Brienza (zb5)
 * Aaron Wisner (daw268)
*)

open String

(* Number base for the string form of the keys *)
type key = Big_int.big_int

let chunk_size = 128
let key_size = 200
let max_length = 2*key_size + 1
let chunk_size_char = Char.chr chunk_size |> String.make 1


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
let of_string = Big_int.big_int_of_string


let b = of_int 256
let exp = of_int 17

(* hashing stuff *)
let key_equal = eq
let key_hash k = bMod k (of_int max_int) |> to_int


(***** HELPERS *****)

(* Requires: s:string of length >0*)
(* Returns: the highest index character in s*)
let last_char s =
  get s (length s - 1)

(* Requires: s:string of length >0*)
(* Returns: s without its highest index character *)
let strip_last s =
  String.sub s 0 (String.length s - 1)


(* Requires: keys a,b, and m *)
(* Returns: a^b mod m while avoiding overflow *)
let rec mod_exp a b m =
  if eq b zero then of_int 1 else
  if even b then
    let v = add (mod_exp a (half b) m) m in
    let x = bMod v m in bMod (mult x x) m
  else
    let v = add (mod_exp a (decr b) m) m in
    bMod (mult (bMod a m)  (bMod v m)) m


(* Requires: key n *)
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

(* Requires a,m of type key *)
(* Returns: a^(-1) mod m, if it exists. Otherwise raise and exception *)
let modinv a m =
  let (g,x,y) = egcd a m m in
  if not (eq g (of_int 1)) then failwith "No inverse"
    else let x' = bMod x m in
  if Big_int.lt_big_int x' zero then add x' m else x'


(* Given a string of any ascii characters return its
 * Big_int representation by treating a string as a base 256 integer
 * where each character correspons to a digit with its ascii code as value *)
let rec string_to_large_int s =
  if s = "" then zero else
  add_i (last_char s |> Char.code)
    (mult b (string_to_large_int (strip_last s)) )



(* Recursive helper function implemeneting the functionality of
 * large_int_to_string *)
let rec large_int_to_string' n s=
  if eq n zero then s else
  let r = bMod n b in
  large_int_to_string'
  (div (sub n r) b) ((to_int r |> Char.chr |> String.make 1)^s)

(* Given a Big_int convert it to a string representation by converting the
 * Big_int to a base 256 integer and creating a string where character i
 * is the character represented by ascii code of digit i in that base
 * 256 representation of the number. *)
let large_int_to_string n = large_int_to_string' n ""


let string_from_key = large_int_to_string

let key_from_string = string_to_large_int

(***** END HELPERS *****)


(* Helper for the large random number generator. Generates a random digit
 * n times, appending them together. *)
let rec big_random' acc n =
  if n = 0 then Big_int.big_int_of_string acc
  else big_random' ((Random.int 10  |> string_of_int)^acc) (n-1)


(* Generate a random very large integer by the helper big_random' for some
 * set number of iterations and appending the results *)
let big_random () =
  big_random' "" key_size


(* Call the large random number generator repeatedly, check whether
 * it is prime, and if not search for another number. Once a prime is
 * found, return it. *)
let generate_key =
  Random.self_init ();
  let rec loop () =
    let p = big_random () in
    if is_prime p && not (eq (bMod (decr p) exp) zero) then p else loop ()
  in loop

(* Generate two large prime number and let their product *)
let generate_public_private ()=
  let x = generate_key () in
  let y = generate_key () in
  (mult x y ,x)

(* Pad to make sure length = max_length *)
let encrypt_line s pu =
  mod_exp (string_to_large_int s) exp pu |> large_int_to_string

let decrypt_line s pu pr =
  let pr' = div pu pr in
  let k' = modinv exp (mult (decr pr') (decr pr)) in
  let s' = string_to_large_int s in
  mod_exp s' k' pu |> large_int_to_string


let compare = eq


let chunk s = String.sub s 0 chunk_size
let remaining s = String.sub s chunk_size (String.length s - chunk_size)
let remaining_dec s = String.sub s max_length (String.length s - max_length)



(* Pads zero characters onto s until len(S) = l *)
let rec zero_pad s l =
  if String.length s < l then
  zero_pad ((String.make 1 '\000')^s) l
  else s



let rec encrypt_and_chunk s pu =
  if String.length s >= chunk_size then
    let enc = zero_pad (encrypt_line (chunk s) pu) max_length in

    let s1 = chunk_size_char ^ enc in
    let s2 = (s |> remaining |> encrypt_and_chunk) pu in s1^s2
  else (String.length s |> Char.chr |> String.make 1)^
    zero_pad (encrypt_line s pu) max_length



let rec decrypt_chunked s pu pr =
  if String.length s >= (max_length + 1) then let size = Char.code s.[0] in
    let dec = decrypt_line (String.sub s 1 max_length) pu pr in
    let s1 = if size = String.length dec then dec else zero_pad dec size in
    let strip = String.sub s 1 (String.length s - 1) |> remaining_dec in
    s1^(decrypt_chunked strip pu pr)
  else if s = "" then s else
  let size = Char.code s.[0] in let dec = decrypt_line (String.sub s 1 (String.length s  - 1)) pu pr in
    if size <> String.length dec then zero_pad dec size else dec


(*

let rec test_modinv n =
  if n = 0 then print_endline "NICE" else
  let (pu,pr) = generate_public_private () in
  let pr2 = div pu pr in
    let x = modinv (of_int 17) (mult (decr pr2) (decr pr)) in
    test_modinv (n-1)
*)


(*

let rec chunk' s acc n=
  if s = "" then acc else
  if String.length s <= n then List.rev (s::acc)  else
  let all_but_n str = String.sub str n (String.length str - n) in

  chunk' (all_but_n s) ((String.sub s 0 n)::acc) n

Given a string return a list of the chunks
let chunk s = chunk' s [] 5 *)
