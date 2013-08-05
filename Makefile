#!/usr/bin/make -f

SOURCES := \
	term.ml \
	parser.mly \
	lexer.mll \
	main.ml

RESULT := proofline0

OCAMLFLAGS := -w A

include OCamlMakefile

