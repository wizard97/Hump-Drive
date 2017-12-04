(* Represents the database's internal state *)

module OUnix = Unix

type state_info
type dir_path

val state_for_dir : dir_path -> state_info Async.Deferred.t

val update_state : state_info -> state_info Async.Deferred.t

val files_to_request : state_info -> state_info -> dir_path list

val acknowledge_file_recpt : state_info ->  string -> state_info Async.Deferred.t

val to_string : state_info -> dir_path

val from_string : dir_path -> state_info
