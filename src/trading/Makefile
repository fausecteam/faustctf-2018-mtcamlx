#!/usr/bin/make -f

PACKAGES = -package http -package zarith -package sha -package yojson

%.cmx: %.ml
	ocamlfind ocamlopt $(PACKAGES) -I ../lib -o $@ -c $<

trading: trading.cmx
	ocamlfind ocamlopt $(PACKAGES) -I ../lib -o $@ -linkpkg faust.cmxa $^

clean:
	rm --force trading *.cmi *.cmo *.cmx
