(* Done to differentiate between OCaml's default Unix module and JaneStreet's Async.Unix *)
module OUnix = Unix
open Async

module StringSet = Set.Make(String)
module FileMap = Map.Make(String)

type file_hash = int
type dir_path = string
type update_queue = StringSet.t
type last_modified = float
type files_to_info = (file_hash * last_modified) FileMap.t

let compute_hash s =
  let hash = ref 0 in
  let update_hash c =
    let h = !hash in
    let h' = ((h lsl 5) - h) + (Char.code c) in
    hash := h' land h'
  in String.iter (update_hash) s; !hash

type state_info = {dir_path : dir_path;
                   files_to_info : files_to_info;
                   last_modified : last_modified;
                   update_queue : update_queue}

let rec get_dir_contents acc h =
  Async.Unix.readdir_opt h >>= (fun s ->
    match s with
    | Some s -> get_dir_contents (s::acc) h
    | None -> Async.Unix.closedir h >>= (fun () -> Deferred.return (acc))
  )

let hash_file fpath =
  Reader.open_file fpath >>= (fun rdr ->
    Reader.pipe rdr |> Async_unix.Import.Pipe.read >>=
    (fun x ->
      match x with
        | `Ok s -> Deferred.return (compute_hash s)
        | `Eof -> failwith "Unsupported")
    )

let is_reg_file fpath =
  let fdesc = OUnix.openfile fpath [O_RDONLY; O_NONBLOCK] 644 in
  let stats = OUnix.fstat fdesc in
  stats.st_kind = S_REG

let last_modtime path =
  let fdesc = OUnix.openfile path [O_RDONLY; O_NONBLOCK] 644 in
  let stats = OUnix.fstat fdesc in
  stats.st_mtime

let files_in_dir dir_path =
  let handle = OUnix.opendir dir_path in
  get_dir_contents [] handle >>= fun lst ->
  Deferred.return (List.filter (fun f -> is_reg_file (dir_path^Filename.dir_sep^f)) lst)

(* NOTE Delete Isn't supported right now. Might want to think about adding some kind of purge function or something.*)

(* Init for a new state given a dir_path *)
let state_for_dir dir_path =
  (files_in_dir dir_path) >>=
  fun filenames ->
    let filehashed' = List.map (fun fil -> hash_file (dir_path^Filename.dir_sep^fil)) filenames in
    let unwrap_and_cons = fun acc i -> i >>= fun e -> acc >>= fun lst -> Deferred.return (e::lst) in
    List.fold_left unwrap_and_cons (Deferred.return []) filehashed' >>=
      fun filehashes ->
        let filemodtimes = List.map (fun fil -> last_modtime
                          (dir_path^Filename.dir_sep^fil)) filenames in
        let file_info = List.map2 (fun a b -> (a,b)) filehashes filemodtimes in
        let file_mappings = List.fold_left2 (fun acc fname finfo -> FileMap.add fname finfo acc)
          FileMap.empty filenames file_info in
        let update_queue = FileMap.fold (fun key _ acc -> StringSet.add key acc) file_mappings StringSet.empty in
        let time = last_modtime dir_path in
        Deferred.return
        {dir_path = dir_path;
         files_to_info = file_mappings;
         last_modified = time;
         update_queue = update_queue}

(* Updates a filemap and queue given current file info  *)
let changed_files dir_path acc (fname, modtime) =
  acc >>= fun (file_map,queue) ->
  try
    let stored_hash, stored_modtime = FileMap.find fname file_map in
    if modtime <> stored_modtime then
      hash_file (dir_path^Filename.dir_sep^fname) >>=
      fun new_hash -> Deferred.return
      (FileMap.add fname (new_hash, modtime) file_map, StringSet.add fname queue)
    else Deferred.return (file_map, queue)
  with Not_found ->
    hash_file (dir_path^Filename.dir_sep^fname) >>=
    fun new_hash -> Deferred.return
    (FileMap.add fname (new_hash, modtime) file_map, StringSet.add fname queue)

(* Given a st, returns an updated filebinding and queue. Helper for update_state  *)
let update_file_info st =
  let dir_path = st.dir_path in
  let file_binds = st.files_to_info in
  let queue = st.update_queue in
  files_in_dir dir_path >>=
    fun curr_dir_contents ->
  let fnames_to_modtimes = List.map (fun fil ->
      (fil, last_modtime (dir_path^Filename.dir_sep^fil))) curr_dir_contents in

  List.fold_left (fun acc x -> changed_files dir_path acc x)
        (Deferred.return (file_binds, queue)) fnames_to_modtimes

let update_state st =
  let dir_path = st.dir_path in
  let new_modtime = last_modtime dir_path in
  if new_modtime <> st.last_modified then
    update_file_info st >>= fun (binds,queue) -> Deferred.return
    {st with files_to_info = binds; update_queue = queue; last_modified = new_modtime}
    else Deferred.return st


let to_string (st : state_info) = Marshal.to_string st []

let from_string (s : string) : state_info = Marshal.from_string s 0
