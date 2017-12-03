(* NOTE main ties together all the parts of our system *)

(* Uses communicator to discover other peers *)
(* Establishes a connection via communicator *)
(* If differences on either this machine or the other, transfer handles updates *)
(* GUI displays all stuff *)
(* Crypto encrpyts files and user info/ connection-establishing processes *)

let main () =
  ANSITerminal.(print_string [green]
                  "\n\nWelcome to Hump Drive. \n");
  print_endline "Searching for
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | s -> print_endline "Hello"




let () = main ()
