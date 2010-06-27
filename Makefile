RESULT = puref
SOURCES = \
	syntax.ml \
	parser.mly \
	lexer.mll \
	main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
