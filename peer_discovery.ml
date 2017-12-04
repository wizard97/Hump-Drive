
(* Module to UDP Packets to other devices one the network
 * to make one's IP
 *)

open Async
open Async_extra
open Async_unix
open Crypto

(* Read public key from some stored file *)
let read_public_key = 0



(* Retrieve the public key as a string *)
let packet =
  "This packet has been received!"
  (* Crypto.generate_public_private () |> fst |> string_of_int *)


let add_user adr s =
  print_endline ("User from"^adr);
  print_endline ("With key"^s);
  failwith "Somebody do this"
(* Read public key and ip address and the*)


(* Send universal broadcast of this device's
 * Note: 255.255.255.255 triggers te device's broadcast address. *)
let broadcast () =
  print_endline "called broadcase";
  let addr1 = Unix_syscalls.Inet_addr.of_string "192.168.1.255" in
  let addr = Async_extra.Import.Socket.Address.Inet.create addr1 12345 in
  let s = Socket.create Socket.Type.udp in
  Socket.connect s addr >>= fun s' ->
    print_endline "got socket";
   let fd = Async_extra.Import.Socket.fd s in
   Socket.setopt s Socket.Opt.broadcast true;
  (Udp.sendto () |> Core.Or_error.ok_exn) fd (Core.Iobuf.of_string packet) addr


(*
  Core.Or_error.ok_exn;
  Core.IoBuff.of_string;
  Socket.fd (socket)
*)



(* Send out UDP packet to alert network of this device
 * Only contains the public key of the device *)
let listen adr =
  print_endline "Called listen";
  let addr1 = Async_unix.Unix_syscalls.Inet_addr.of_string adr in
  let adr_port = Async_extra.Import.Socket.Address.Inet.create addr1 12345 in
    print_endline "Listening...";
  Udp.bind adr_port >>= fun s ->
    print_endline "got socket";
    let fd = Async_extra.Import.Socket.fd s in
      let callback buf adr =
        let adr' = Async_extra.Import.Socket.Address.Inet.to_string adr in
        add_user adr' (Core.Iobuf.to_string buf);
        print_endline "Received all the data" in
  Udp.recvfrom_loop fd callback


 (* Udp.bind addr >>= Async_extra.Import.Socket.listen
   Socket.connect s >>= fun s1 -> *)
