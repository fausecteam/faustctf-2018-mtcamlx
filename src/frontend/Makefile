#!/usr/bin?make -f

all: frontend

sources := $(wildcard *.hs)

frontend: Main.hs $(sources)
	HOME=$(CURDIR)/../../ ghc -hide-package crypto-api -O2 -threaded -o $@ $<

.PHONY: prepare
