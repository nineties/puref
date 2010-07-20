RESULT = puref
SOURCES = \
	syntax.ml \
	parser.mly \
	lexer.mll \
	gmachine.ml \
	compile.ml \
	pprint.ml \
	main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
