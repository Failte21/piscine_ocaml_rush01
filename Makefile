RESULT = rush01
SOURCES = \
	Gui/Gui.ml \
	Creature/Creature.mli \
	Creature/Creature.ml \
	GameState/GameState.mli \
	GameState/GameState.ml \
	main.ml
PACKS = lambda-term
THREADS = true
OCAMLMAKEFILE = ~/.opam/default/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)
