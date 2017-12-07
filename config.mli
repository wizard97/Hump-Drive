open State
open Unix
module OUnix = Unix
open Async
open Async_extra.Import.Reader
open Async_extra.Import.Writer

exception NoSuchFile of string

val dir_CONFIG : string

(* Checks that file or directory denoted by path is valid. Returns path if ok. *)
val path_ok : string -> string


val config_ok : string -> string


val write_file : string -> string -> string -> unit Deferred.t


val fname_PUBKEY : string
val fname_PRIVKEY : string
val fname_PEERS : string
val fname_STORED_STATE : string


(* <==== Declare load files here ====> *)
val load_pubkey : string ->  string Deferred.t
val load_privkey : string ->  string Deferred.t
val load_peerkey : string ->  string Deferred.t

val load_state : string -> State.state_info Deferred.t



(* <==== Write functions ====> *)
val save_state : State.state_info -> string -> unit Async.Deferred.t

val save_st_string : string -> string -> unit Async.Deferred.t
