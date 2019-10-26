RESULT = rush01
SOURCES = \
	src/Gui/Gui.ml \
	src/Creature/Creature.mli \
	src/Creature/Creature.ml \
	src/GameState/GameState.mli \
	src/GameState/GameState.ml \
	src/main.ml
PACKS = lambda-term
THREADS = true
OCAMLMAKEFILE = ~/.opam/default/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)
