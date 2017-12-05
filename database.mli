(* Represents the database's internal state *)

module OUnix = Unix
open Async

type state_info

val state_for_dir : string -> state_info Async.Deferred.t

val update_state : state_info -> state_info Async.Deferred.t

val files_to_request : state_info -> state_info -> string list

val acknowledge_file_recpt : state_info ->  string -> state_info Async.Deferred.t

val to_string : state_info -> string

val from_string : string -> state_info
