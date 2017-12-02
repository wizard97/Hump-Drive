open Async
open Async.Tcp_file

type 'a result = FT_Error of string | FT_Success of 'a


let port = 12345

let serve_auth addr =
  true

let create_server () =
  Server.serve ~auth:(serve_auth) (Tcp.on_port port)


let server_add_file f = Server.serve_existing_static_file f



let client_connect host = Client.connect ~host:(host) ~port:(port)


let rec dq_str q saveloc =
  let save_chunk = function
    | Client.Message.String s -> File_writer.write saveloc s
    | Client.Message.Bigstring bs -> File_writer.write_bigstring saveloc bs
  in
  match (Core_kernel.Std.Queue.dequeue q) with
  | None -> ()
  | Some resp ->
    match (resp) with
    | Error e -> print_string "Someone messed up"
    | Ok msg -> save_chunk msg; dq_str q saveloc


let client_read client fname sfile =
  let rec readentire pipe =
    let pread = Async_extra.Import.Pipe.read' pipe in
    pread >>= fun pq ->
    match (pq) with
    | `Eof -> File_writer.close sfile
    | `Ok q -> dq_str q sfile; readentire pipe
  in
  let dpipe = Client.read client fname in
  dpipe >>= fun pipe -> readentire pipe


(* c is client instance, fname is file to read, saveloc is full path*)
let client_get_file c fname saveloc =
  File_writer.create saveloc >>= fun sfile ->
  c >>= fun (res) ->
  match res with
  | Ok c -> let _ = (client_read c fname sfile) in Async_extra.Import.return (FT_Success ())
  | Error r -> Async_extra.Import.return (FT_Error "Error transfering file")
