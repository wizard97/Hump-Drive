open Async
open Async_extra.Tcp
open Async_extra

type ftclient =
  (Tcp_file.Client.t, Base.Exn.t) Core.Result.t Async_extra.Import.Deferred.t

let port = 12345



let create_server fname =
  let server_callback addr _ write =
    print_string "Got Client!\n";
    Socket.Address.Inet.addr addr |> Unix.Inet_addr.to_string |> print_endline;
    Reader.open_file fname >>= fun fd ->
    Import.Writer.transfer write (Reader.pipe fd)
      (fun s -> Import.Writer.write write s) >>= fun () ->
    print_string "Finished Transferring!";
    Import.Writer.close write
  in
  Server.create (on_port port) server_callback


let client_connect_read host fname =
  connect (to_host_and_port host port) >>= fun (_ , reader, _) ->
  print_string "Connected to server!\n";
  Writer.open_file fname >>= fun fd ->
  Import.Reader.transfer reader (Writer.pipe fd) >>= fun () ->
  print_string "Finished receiving!";
  Import.Reader.close reader
