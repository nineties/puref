RESULT = puref
SOURCES = \
	syntax.ml \
	parser.mly \
	lexer.mll \
	option.ml \
	vmtypes.mli \
	visualize.ml \
	gmachine.ml \
	compile.ml \
	pprint.ml \
	main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
