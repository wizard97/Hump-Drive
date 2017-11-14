compile:
	ocamlbuild -use-ocamlfind communicator.cmo crypto.cmo gui.cmo transfer.cmo

clean:
	ocamlbuild -clean
