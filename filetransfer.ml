open Async
open Async.Tcp_file

type 'a result = FT_Error of string | FT_Success of 'a

type ftclient =
  (Tcp_file.Client.t, Base.Exn.t) Core.Result.t Async_extra.Import.Deferred.t

let port = 12345

let serve_auth addr =
  print_string "Got Client!";
  true

let create_server () =
  print_string "Starting!";
  Server.serve ~auth:(serve_auth) (Tcp.on_port port)


let server_add_file f = Server.serve_existing_static_file f



let client_connect host = Client.connect ~host:(host) ~port:(port)


let rec dq_str q saveloc =
  (* Why do we need to add a newline!!!*)
  let save_chunk = function
    | Client.Message.String s ->
      File_writer.write saveloc s
    | Client.Message.Bigstring bs ->
      File_writer.write_bigstring saveloc bs
  in
  match q with
    | Error e -> print_string "Error!"; print_string (Client.Error.to_string e)
    | Ok msg -> save_chunk msg; dq_str q saveloc


(*
  | None -> ()
  | Some resp ->
    match (resp) with
    | Error e -> print_string "Error!"; print_string (Client.Error.to_string e)
    | Ok msg -> save_chunk msg; dq_str q saveloc
*)

let client_read client fname sfile =
  let rec readentire pipe =
    let pread = Async_extra.Import.Pipe.read pipe in
    pread >>= fun pq ->
    match (pq) with
    | `Eof -> File_writer.close sfile
    | `Ok q -> dq_str q sfile; readentire pipe
    (*File_writer.write sfile (Core_kernel.Result.ok_exn q); readentire pipe *)
(*| `Ok q -> dq_str q sfile; readentire pipe *)

  in
  let dpipe = Client.read client fname in
  dpipe >>= fun pipe -> readentire pipe


(* c is client instance, fname is file to read, saveloc is full path*)
let client_get_file c fname saveloc =
  File_writer.create ~append:(false) saveloc >>= fun sfile ->
  c >>= fun (res) ->
  match res with
  | Ok c -> let _ = (client_read c fname sfile) in Async_extra.Import.return (FT_Success ())
  | Error r -> Async_extra.Import.return (FT_Error "Error transfering file")
