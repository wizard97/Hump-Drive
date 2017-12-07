(* Scott Dickson
 * 12/2/2017
 * Test file for the cryptography section
*)

open OUnit2
open Crypto

(* Aliases used by crypto.ml *)

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


(*End functions  *)

let test () =
  let (pu,pr) = Crypto.generate_public_private () in
  let l1 = encrypt_and_chunk "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" pu in

  (* let l1 = encrypt_and_chunk "small" pu in *)
  (* let l1 = encrypt_and_chunk "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAaAAA" pu in *)
  "After encryption: "^l1 |> print_endline;
  print_string "Length is: "; print_int (String.length l1);
  let l2 = decrypt_chunked l1 pu pr in
  "After Decryption: "^l2 |> print_endline;
  print_string "Length is: "; print_int (String.length l2)
(*

let rec test_string_to_key n =
  if n = 10 then print_endline "NICE" else

  let (pu,pr) = Crypto.generate_public_private () in
  let str1 =Crypto.string_from_key pu in
  let str2 = Crypto.key_from_string str1 in
  if (eq str2 pu) then test_string_to_key (n+1) else print_endline "CRAP"

let test_mods () =

  let (pu,pr) = Crypto.generate_public_private () in
  let a = of_int (190*190) in
  let b = of_int 191 in
  let m = pu in
  let val1 = mod_exp a b m in
  let val2 = modinv b (mult (decr pr) pr) in
  print_string "Value is: ";
  print_string (Big_int.string_of_big_int (mod_exp val1 val2 m));
  print_endline ""
*)
let _ = test ()

