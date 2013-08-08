#!/usr/bin/make -f

SOURCES := \
	misc.ml \
	term.ml \
	environment.ml \
	instruction.ml \
	parser.mly \
	lexer.mll \
	main.ml

RESULT := proofline0

OCAMLFLAGS := -w A

include OCamlMakefile

