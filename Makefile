compile:
	ocamlbuild -use-ocamlfind communicator.cmo crypto.cmo gui.cmo transfer.cmo
crypto_test:
	ocamlbuild -use-ocamlfind crypto_test.byte && ./crypto_test.byte
clean:
	ocamlbuild -clean
