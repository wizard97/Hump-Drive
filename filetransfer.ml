open Async
open Async_extra.Tcp

type 'a result = FT_Error of string | FT_Success of 'a

type ftclient =
  (Tcp_file.Client.t, Base.Exn.t) Core.Result.t Async_extra.Import.Deferred.t

let port = 12345

let serve_auth addr =
  print_string "Got Client!";
  true



let create_server fname =
  let server_callback addr _ write =
    print_string "Got Client!";
    Socket.Address.Inet.addr addr |> Unix.Inet_addr.to_string |> print_string;
    let f = Reader.open_file fname in
    f >>= fun fd ->
    Async_extra.Import.Writer.transfer write (Reader.pipe fd)
      (fun s -> Async_extra.Import.Writer.write write s) >>= fun () ->
    Async_extra.Import.Writer.close write
  in
  Server.create (on_port port) server_callback




let client_connect_read host fname =
  connect (to_host_and_port host port) >>= fun (sock, reader, writer) ->
  print_string "Connected to server!";
  let f = Writer.open_file fname in
  f >>= fun fd -> Async_extra.Import.Reader.transfer reader (Writer.pipe fd) >>= fun () ->
  Async_extra.Import.Reader.close reader
