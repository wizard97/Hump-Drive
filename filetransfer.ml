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


let rec dq_str q =
  match (Core_kernel.Std.Queue.dequeue q) with
  | None -> ()
  | Some resp ->
    match (resp) with
    | Error e -> ()
    | Ok msg -> let () = print_string (Client.Message.to_string_exn msg) in dq_str q

let client_read client fname =
  let cr = Client.read client fname in
  let pr = cr >>= fun pipe -> Async_extra.Import.Pipe.read' pipe in
  let res = pr >>= fun rd -> match rd with
    | `Eof -> let () = print_string "EOF" in Async_extra.Import.return ()
    | `Ok q -> let () = dq_str q in Async_extra.Import.return ()
  in
  res


(* c is client instance, fname is file to read, saveloc is full path*)
let client_get_file c fname saveloc =
  c >>= fun (res) ->
  match res with
  | Ok c -> let _ = (client_read c fname) in Async_extra.Import.return (FT_Success ())
  | Error r -> Async_extra.Import.return (FT_Error "Error transfering file")
