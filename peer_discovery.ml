
(* Module to UDP Packets to other devices one the network
 * to make one's IP
 *)

open Async
open Async_unix


type listen_state = Fd.t


let port = 30301



(* Send universal broadcast of this device's
 * Note: 255.255.255.255 triggers te device's broadcast address. *)
let broadcast msg =
  let addr1 = Unix_syscalls.Inet_addr.of_string "255.255.255.255" in
  let addr = Async_extra.Import.Socket.Address.Inet.create addr1 port in
  let s = Socket.create Socket.Type.udp in
  Socket.setopt s Socket.Opt.broadcast true;
  Socket.connect s addr >>= fun s' ->
   let fd = Async_extra.Import.Socket.fd s in
   (Udp.sendto () |> Core.Or_error.ok_exn) fd (Core.Iobuf.of_string msg) addr >>= fun _ ->
   Socket.shutdown s Socket.(`Both);
   Deferred.return ()



(*
  Core.Or_error.ok_exn;
  Core.IoBuff.of_string;
  Socket.fd (socket)
*)


let split_port s =
  let idx = String.index_from s 0 ':' in
  String.sub s 0 idx


(* Send out UDP packet to alert network of this device
 * Only contains the public key of the device *)
let listen dcallback =
  let adr_port = Async_extra.Import.Socket.Address.Inet.create_bind_any port in
  Udp.bind adr_port >>= fun s ->
    let fd = Async_extra.Import.Socket.fd s in
    let callback buf adr =
      let adrs = Async_extra.Import.Socket.Address.Inet.to_string adr in
      dcallback (split_port adrs) (Core.Iobuf.to_string buf)
    in
    Udp.recvfrom_loop fd callback >>= fun () -> Deferred.return fd


 (* Udp.bind addr >>= Async_extra.Import.Socket.listen
   Socket.connect s >>= fun s1 -> *)
