RESULT = main
SOURCES = \
	Gui/Gui.ml \
	Creature/Creature.mli \
	Creature/Creature.ml \
	GameState/GameState.mli \
	GameState/GameState.ml \
	main.ml
PACKS = lambda-term
OCAMLMAKEFILE = ~/.opam/default/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)