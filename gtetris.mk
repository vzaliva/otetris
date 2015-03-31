PACKS = batteries sdl.sdlimage sdl.sdlmixer sdl.sdlttf sdl.sdlgfx sdl
RESULT     = gtetris
SOURCES    = tetris.ml gtetris.ml
LIBS       = batteries sdl
INCDIRS    = $(HOME)/.opam/system/lib/sdl
OCAMLLDFLAGS = -cclib "-framework Cocoa"
THREADS = true

include OCamlMakefile

