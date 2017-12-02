(* Scott Dickson
 * 12/2/2017
 * Test file for the cryptography section
*)

open OUnit2
open Crypto


let test () =
  let (pu,pr) = Crypto.generate_public_private () in
  let l1 = encrypt_line "WHAT" pu in
  let l2 = decrypt_line l1 pu pr in
  print_endline l2

let _ = test ()

