open Unix

(* <==== Declare filename constants here ====> *)
let fname_PUBKEY = ".pubkey"

(* Checks that file or directory denoted by path is valid. Returns path if ok. *)
let path_ok path =
  let _ = openfile path [O_RDONLY] 644 |> close in
  path

(* Checks that config directory exists in the given dir_path. *)
let config_dir dir_path =
  try
    let path = dir_path^Filename.dir_sep^"config" in
    path_ok path
  with Unix.Unix_error _ -> failwith "Config directory is not established. Please create a directory with name /config"

(* Loads in the contents of <par_path>/config/<fname> in as a string.  *)
let load_file fname par_path =
  let config_path = config_dir par_path in
  let fpath = config_path^Filename.dir_sep^fname in
  try
    let _ = path_ok fpath in
    let f_handle = openfile fpath [O_RDONLY] 644 in
    let in_chan = in_channel_of_descr f_handle in
    really_input_string in_chan (in_channel_length in_chan)
  with Unix.Unix_error _ ->
    failwith ("Error loading file of name: " ^ fname ^". Please make sure that file exists in /config")

(* <==== Declare load files here ====> *)
(* Loads in the contents of the pubkey as a string *)
let load_pubkey = load_file fname_PUBKEY
