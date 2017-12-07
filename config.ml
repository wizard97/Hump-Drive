open State
open Unix
module OUnix = Unix
open Async
open Async_extra.Import.Reader
open Async_extra.Import.Writer

exception NoSuchFile of string

let dir_CONFIG = ".config"

(* Checks that file or directory denoted by path is valid. Returns path if ok. *)
let path_ok path = OUnix.access path [F_OK]; path

(* Checks that config directory exists in the given dir_path. *)
let config_ok dir_path =
  let path = dir_path^Filename.dir_sep^dir_CONFIG in
  try path_ok path
  with OUnix.Unix_error _ ->
    OUnix.mkdir path 0o777 ;path

let load_file fname dir_path =
   let config_path = config_ok dir_path in
   let fpath = config_path^Filename.dir_sep^fname in
   try
     let _ = path_ok fpath in
     Reader.file_contents fpath

   with OUnix.Unix_error _ ->
     raise (NoSuchFile("Error loading config file of name: " ^fname ^". Please make sure that file exists."))

let write_file s fname dir_path =
  let config_path = config_ok dir_path in
  let fpath = config_path^Filename.dir_sep^fname in
  Writer.open_file fpath >>= fun fd -> write fd s; Deferred.return ()

(* <==== Declare filename constants here ====> *)
let fname_PUBKEY = ".pubkey"
let fname_PRIVKEY = ".privkey"
let fname_STORED_STATE = ".state"

(* <==== Declare load files here ====> *)
let load_pubkey = load_file fname_PUBKEY
let load_state dir =
  load_file fname_STORED_STATE dir >>= fun s ->
  Deferred.return (State.from_string s)

(* <==== Write functions ====> *)
let save_state st dir_path =
  let s = State.to_string st in
  write_file s fname_STORED_STATE dir_path
