compile:
	ocamlbuild -use-ocamlfind communicator.cmo crypto.cmo gui.cmo
	corebuild -pkg async filetransfer.cmo communication.cmo peer_discovery.cmo
crypto_test:
	corebuild -pkg async crypto_test.byte && ./crypto_test.byte
net_test_server:
	corebuild -pkg async net_test_server.byte && ./net_test_server.byte
net_test_client:
	corebuild -pkg async net_test_client.byte && ./net_test_client.byte
clean:
	ocamlbuild -clean
