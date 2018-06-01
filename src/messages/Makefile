#!/usr/bin/make -f

PACKAGES = -thread -package http -package sqlexpr,sqlexpr.syntax -package yojson -syntax camlp4o -package zarith -package sha

%.cmx: %.ml
	ocamlfind ocamlopt $(PACKAGES) -I ../lib -o $@ -c $<

messages: messages.cmx
	ocamlfind ocamlopt $(PACKAGES) -I ../lib -o $@ -linkpkg faust.cmxa $^

clean:
	rm --force messages *.cmi *.cmo *.cmx
