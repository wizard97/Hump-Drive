(* Representation for interactable objects in the GUI window *)
type window_obj

(* Creates a new GUI window *)
val make_window : unit -> unit

(* What happens to a window on user click *)
val click : unit -> unit

(* Recieves user input text to GUI *)
val input_text : unit -> unit

(* Callback for GUI to listen for an action's response *)
val wait_for_response : unit -> unit

(* Updates GUI/Window *)
val update : unit -> unit

(* Exit GUI gracefully *)
val quit : unit -> unit
