open Unix

module StringSet = Set.Make(String)
module FileMap = Map.Make(String)

type file_hash = int64
type dir_path = string
type update_queue = StringSet.t
type last_modified = float (* NOTE - thats what Unix uses for stats *)
type files_to_hash = file_hash FileMap.t

let compute_hash s = Int64.zero

(* NOTE: For now, just doing one directory to make things easier*)
(* type state = Dir of state list * file_to_hash * dir_hash *)
type state = Dir of dir_path * files_to_hash * last_modified * update_queue

let rec get_dir_contents acc h =
  match Unix.readdir h with
  | s -> get_dir_contents (s::acc) h
  | exception End_of_file -> Unix.closedir h; acc

let hash_file fname dirname=
  let fpath = dirname^Filename.dir_sep^fname in
  let file = open_in fpath in
  let f_len = in_channel_length file in
  let f_contents = really_input_string file f_len in
  compute_hash f_contents (* XOR every single byte *)

let is_reg_file fpath =
  let fdesc = openfile fpath [O_RDONLY] 644 in
  let stats = fstat fdesc in
  stats.st_kind = S_REG

let dir_last_modtime dir_path =
  let fdesc = openfile dir_path [O_RDONLY] 644 in
  let stats = fstat fdesc in
  stats.st_mtime

let state_for_dir dir_path =
  let handle = opendir dir_path in
  let dir_contents = get_dir_contents [] handle in
  let filenames = List.filter (fun f -> is_reg_file (dir_path^Filename.dir_sep^f))
      dir_contents in
  let filehashes = List.map (fun fil -> hash_file fil dir_path) filenames in
  let file_mappings =
      List.fold_left2 (fun acc fname fhash -> FileMap.add fname fhash acc)
        FileMap.empty filenames filehashes in
  let update_queue = FileMap.fold (fun key _ acc -> StringSet.add key acc) file_mappings StringSet.empty in
  let time = dir_last_modtime dir_path in
  Dir(dir_path, file_mappings, time, update_queue)



  (* let fnames_to_hash = List.combine f_names f_hashes in
  let fhashes_mega_str = List.fold_left (fun acc x -> acc ^ x) "" f_hashes in
  let d_hash = compute_hash fhashes_mega_str in *)

(* let update_state st =
  match st with
  | dir_path, _ , _ -> state_for_dir dir_path *)
