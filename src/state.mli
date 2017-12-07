(* OCaml's standard Unix is opened as OUnix to disambiguate from Async's Unix module  *)
open Unix
module OUnix = Unix
open Async

(* Abstract type used to maintain a given directory's database state *)
type state_info

(* [state_for_dir dpath] Returns a Deferred state_info of a newly initialized
 * state_info for the specified [dpath]. *)
val state_for_dir : string -> state_info Async.Deferred.t

(* [update_state st] Returns a Deferred state_info of an updated state for the
 * specified state [st]. This function should be used to keep local changes in
 * the tracked directory synced with the database state_info represented by [st] *)
val update_state : state_info -> state_info Async.Deferred.t

(* [files_to_request st1 st2] compares two state_info's and returns a string list
 * of files that st1 needs to request from st2 in order to be synced up to date. *)
val files_to_request : state_info -> state_info -> string list

(* [acknowledge_file_recpt st fname] Returns a deferred state_info that represents
 * an updated [st] for the file of name [fname]. This function is intended to be
 * used as a callback to update a machine's state_info when a requested file
 * denoted by [fname] is successfully received by the machine  *)
val acknowledge_file_recpt : state_info ->  string -> state_info Async.Deferred.t

(* Exports a state_info to a non-human readable string *)
val to_string : state_info -> string

(* Imports a string representing a state_info as a state_info *)
val from_string : string -> state_info

(* Returns the root directory that this system with search for files from *)
val root_dir : state_info -> string
