(* OCaml's standard Unix is opened as OUnix to disambiguate from Async's Unix module  *)
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


val files_to_request : state_info -> state_info -> string list

val acknowledge_file_recpt : state_info ->  string -> state_info Async.Deferred.t

val to_string : state_info -> string

val from_string : string -> state_info
