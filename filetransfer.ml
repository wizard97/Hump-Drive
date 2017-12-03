open Async
open Async_extra.Tcp

type ftclient =
  (Tcp_file.Client.t, Base.Exn.t) Core.Result.t Async_extra.Import.Deferred.t

let port = 12345


let create_server fname =
  let server_callback addr _ write =
    print_string "Got Client!";
    Socket.Address.Inet.addr addr |> Unix.Inet_addr.to_string |> print_endline;
    let f = Reader.open_file fname in
    f >>= fun fd ->
    Async_extra.Import.Writer.transfer write (Reader.pipe fd)
      (fun s -> Async_extra.Import.Writer.write write s) >>= fun () ->
    print_string "Finished Transferring!";
    Async_extra.Import.Writer.close write
  in
  Server.create (on_port port) server_callback




let client_connect_read host fname =
  connect (to_host_and_port host port) >>= fun (sock, reader, _) ->
  print_string "Connected to server!";
  let f = Writer.open_file fname in
  f >>= fun fd -> Async_extra.Import.Reader.transfer reader (Writer.pipe fd) >>= fun () ->
  print_string "Finished receiving!";
  Async_extra.Import.Reader.close reader
