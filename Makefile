RESULT = puref
SOURCES = \
	syntax.ml \
	parser.mly \
	lexer.mll \
	pprint.ml \
	main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
