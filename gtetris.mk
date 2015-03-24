PACKS = batteries
RESULT     = gtetris
SOURCES    = tetris.ml gteris.ml
LIBS       = bigarray batteries sdl
INCDIRS    = $(HOME)/.opam/system/lib/sdl

include OCamlMakefile

