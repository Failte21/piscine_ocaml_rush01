RESULT = rush01
SOURCES = \
	src/Action/Action.mli \
	src/Action/Action.ml \
	src/Gui/Gui.ml \
	src/Creature/Creature.mli \
	src/Creature/Creature.ml \
	src/GameState/GameState.mli \
	src/GameState/GameState.ml \
<<<<<<< HEAD
	src/Backup/Backup.mli \
	src/Backup/Backup.ml \
=======
	src/Animation/Animation.mli \
	src/Animation/Animation.ml \
>>>>>>> add animation
	src/main.ml
PACKS = lambda-term
THREADS = true
OCAMLMAKEFILE = ~/.opam/default/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)
