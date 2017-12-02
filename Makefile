compile:
	ocamlbuild -use-ocamlfind communicator.cmo crypto.cmo gui.cmo filetransfer.ml
crypto_test:
	ocamlbuild -use-ocamlfind crypto_test.byte && ./crypto_test.byte
net_test:
	ocamlbuild -use-ocamlfind net_test.byte && ./net_test.byte
clean:
	ocamlbuild -clean
