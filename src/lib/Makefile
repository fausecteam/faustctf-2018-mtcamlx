#!/usr/bin/make -f

PACKAGES = -package zarith -package oUnit -package yojson -package fieldslib -package sha

all: test faust.cmxa

%.cmx: %.ml
	[ ! -f $(patsubst %ml,%mli,$<) ] || ocamlfind ocamlopt $(PACKAGES) -c $(patsubst %ml,%mli,$<)
	ocamlfind ocamlopt $(PACKAGES) -o $@ -c $<

faust.cmxa: faustMath.cmx faustAuthenticate.cmx
	ocamlfind ocamlopt $(PACKAGES) -a -o $@ $^

test: faust.cmxa test.cmx
	ocamlfind ocamlopt $(PACKAGES) -o $@ -linkpkg $^

clean:
	rm --force test *.cmi *.cmo *.cmx

check: test
	./test
