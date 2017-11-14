(* This module is for transfering files between peers using TCP *)
open Unix
open Crypto


(* Send information such as when it was updated, etc... *)
type file_metadata

(* Another *)
type peer

(* Create session with peer*)
type session

(* Establish an intial coomunication session with peer *)
val connect : peer -> session option


(* Accept an incomming connection request within a timeout *)
val accept : int -> session option


(* Send a file with name string to the other side *)
val send_file : file_metadata -> string -> session -> bool


(* Recieve an incoming file and save it to a location *)
val receive_file : string -> string -> session -> file_metadata option
