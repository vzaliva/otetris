PACKS = batteries sdl.sdlimage sdl.sdlmixer sdl.sdlttf sdl
RESULT     = gtetris
SOURCES    = tetris.ml gtetris.ml
LIBS       = Bigarray batteries sdl
INCDIRS    = $(HOME)/.opam/system/lib/sdl
OCAMLLDFLAGS = -cclib "-framework Cocoa"
THREADS = true

include OCamlMakefile

