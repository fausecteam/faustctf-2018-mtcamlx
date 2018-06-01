#!/usr/bin/make -f

PACKAGES = -package http -package zarith -package sha -package yojson

all: keygen authentication

%.cmx: %.ml
	ocamlfind ocamlopt $(PACKAGES) -I ../lib -o $@ -c $<

authentication: authentication.cmx
	ocamlfind ocamlopt $(PACKAGES) -I ../lib -o $@ -linkpkg faust.cmxa $^

keygen: keygen.cmx
	ocamlfind ocamlopt $(PACKAGES) -I ../lib -o $@ -linkpkg faust.cmxa $^


clean:
	rm --force authentication *.cmi *.cmo *.cmx
