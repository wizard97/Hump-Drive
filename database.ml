open Unix

module StringSet = Set.Make(String)
module FileMap = Map.Make(String)

type file_hash = string
type dir_path = string
type update_queue = StringSet.t
type last_modified = int64
type files_to_hash = file_hash FileMap.t

let compute_hash s = 0

(* NOTE: For now, just doing one directory to make things easier*)
(* type state = Dir of state list * file_to_hash * dir_hash *)
type state = Dir of dir_path * files_to_hash * last_modified * update_queue

let rec get_doc_fnames acc h =
  match Unix.readdir h with
  | s -> get_doc_fnames (s::acc) h
  | exception End_of_file -> Unix.closedir h; acc

(* let hash_file fname dirname=
  let fpath = dirname^Filename.dir_sep^fname in
  let file = open_in fpath in
  let f_len = in_channel_length file in
  let f_contents = really_input_string file f_len in
  compute_hash f_contents (* XOR every single byte *) *)

(* let state_for_dir dir_path =
  let handle = opendir dir_path in
  let f_names = get_doc_fnames [] handle in
  let f_hashes = List.map (fun fil -> hash_file fil dir_path) f_names in
  let fnames_to_hash = List.combine f_names f_hashes in
  let fhashes_mega_str = List.fold_left (fun acc x -> acc ^ x) "" f_hashes in
  let d_hash = compute_hash fhashes_mega_str in
  Dir (dir_path, fnames_to_hash, d_hash) *)

(* let update_state st =
  match st with
  | dir_path, _ , _ -> state_for_dir dir_path *)
