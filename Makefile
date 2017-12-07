compile:
	ocamlbuild -use-ocamlfind crypto.cmo
	corebuild -pkg async filetransfer.cmo communication.cmo peer_discovery.cmo state.cmo
crypto_test:
	corebuild -pkg async crypto_test.byte && ./crypto_test.byte
net_test_server:
	corebuild -pkg async net_test_server.byte && ./net_test_server.byte
net_test_client:
	corebuild -pkg async net_test_client.byte && ./net_test_client.byte
peer_broadcast:
	corebuild -pkg async discovery_test.byte && ./discovery_test.byte
peer_listen:
	corebuild -pkg async discover_listen.byte && ./discover_listen.byte
repl:
	corebuild -pkg async main.byte && ./main.byte
state:
	corebuild -pkg async state.byte && ./state.byte
clean:
	ocamlbuild -clean
