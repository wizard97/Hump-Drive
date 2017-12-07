compile:
	ocamlbuild -use-ocamlfind crypto.cmo
	corebuild -pkg async communication.cmo peer_discovery.cmo state.cmo config.cmo
crypto_test:
	corebuild -pkg async crypto_test.byte && ./crypto_test.byte
repl:
	corebuild -pkg async main.byte && ./main.byte
clean:
	ocamlbuild -clean
