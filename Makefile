RESULT = main
SOURCES = \
	Gui/Gui.ml \
	main.ml
PACKS = lambda-term
OCAMLMAKEFILE = ~/.opam/default/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)