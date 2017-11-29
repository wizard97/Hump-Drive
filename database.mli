(* Represents the database's internal state *)
type state

(* Represents a filepath *)
type filepath

(* Represents the state of the empty database *)
val empty_db: state

(* Add a file to be tracked to the state *)
val add_file: filepath -> state -> state

(* Removes a file from state *)
val remove_file: string -> state -> state

(* Updates a file's representation in state if the file has been changed *)
val update_file: filepath -> state -> state

(* Computes a hash for a given file *)
val compute_hash: string -> string

(* Compares two database states.*)
val states_equiv: state -> state -> bool

(* Formats state to a human-readable string *)
val to_string : state -> string
