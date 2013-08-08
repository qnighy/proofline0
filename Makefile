#!/usr/bin/make -f

SOURCES := \
	term.ml \
	environment.ml \
	parser.mly \
	lexer.mll \
	main.ml

RESULT := proofline0

OCAMLFLAGS := -w A

include OCamlMakefile

