compile:
	ocamlbuild -use-ocamlfind communicator.cmo crypto.cmo gui.cmo filetransfer.ml
crypto_test:
	corebuild -pkg async crypto_test.byte && ./crypto_test.byte
net_test_server:
	corebuild -pkg async net_test_server.byte && ./net_test_server.byte
net_test_server:
	corebuild -pkg async net_test.byte && ./net_test.byte
clean:
	ocamlbuild -clean
