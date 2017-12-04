(* Represents the database's internal state *)

module OUnix = Unix
open Async

type state_info
type dir_path

val state_for_dir : dir_path -> state_info Deferred.t

val update_state : state_info -> state_info Deferred.t

val to_string : state_info -> dir_path

val from_string : dir_path -> state_info

