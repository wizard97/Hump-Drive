open Async
open Async.Tcp_file

let port = 12345

let serve_auth addr =
  true

let create_server () =
  Server.serve ~auth:(serve_auth) (Tcp.on_port port)


let server_add_file f = Server.serve_existing_static_file f


let server_run () =
  create_server () >>= fun s -> server_add_file "test.txt"

let client_connect () = Client.connect ~host:("127.0.0.1") ~port:(port)


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


let client_work () =
  client_connect () >>=
  fun res -> match (res) with
  | Ok c -> let _ = (client_read c "test.txt") in Async_extra.Import.return ()
  | Error r -> Async_extra.Import.return ()
