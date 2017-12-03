
(*
-broadcast address
ends in 255

-Upd packet thorugh that containing public key
-on receiving side and record ip address *)

open Async
open Async_extra
open Async_unix

(* Read public key from some stored file *)
let read_public_key = 0


let broadcast =
  let addr1 = Unix_syscalls.Inet_addr.of_string "255.255.255.255" in
  let addr = Async_extra.Import.Socket.Address.Inet.create addr1 12345 in
  Udp.bind addr >>= fun s ->
   let fd = Async_extra.Import.Socket.fd s in
   (* Socket.setopt s Socket.Opt.broadcast true; *)
  (Udp.sendto () |> Core.Or_error.ok_exn) fd (Core.Iobuf.of_string "Private") addr


(*
  Core.Or_error.ok_exn;
  Core.IoBuff.of_string;
  Socket.fd (socket)
*)



(* Send out UDP packet to alert network of this device
 * Only contains the public key of the device *)
let listen =
  let addr1 = Async_unix.Unix_syscalls.Inet_addr.of_string "127.0.0.1" in
  let addr = Import.Socket.Address.Inet.create addr1 12345 in
  Udp.bind addr >>= Import.Socket.listen
  (* Socket.connect s >>= fun s1 -> *)

