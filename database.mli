(* Represents the database's internal state *)

module OUnix = Unix
open Async

type state_info
type file_hash
type dir_path
type update_queue
type last_modified
type files_to_info

val compute_hash : dir_path -> file_hash


val get_dir_contents : dir_path list -> OUnix.dir_handle -> dir_path list Deferred.t


val hash_file : dir_path -> file_hash Deferred.t

val is_reg_file : dir_path ->


val last_modtime : dir_path -> last_modified

val files_in_dir : dir_path -> dir_path list Deferred.t

val state_for_dir : dir_path -> state_info Deferred.t


val changed_files : dir_path -> ((file_hash * 'a) FileMap.t * update_queue) Deferred.t ->
  dir_path * 'a -> ((file_hash * 'a) FileMap.t * update_queue) Deferred.t


val update_file_info : state_info -> ((file_hash * last_modified) FileMap.t * update_queue) Deferred.t

val update_state : state_info -> state_info Deferred.t

val to_string : state_info -> dir_path

val from_string : dir_path -> state_info

