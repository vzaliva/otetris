PACKS = batteries sdl.sdlimage sdl.sdlmixer sdl.sdlttf sdl.sdlgfx sdl
RESULT     = gtetris
SOURCES    = tetris.ml gtetris.ml
LIBS       = batteries sdl
INCDIRS    = $(HOME)/.opam/system/lib/sdl
THREADS = true

OS := $(shell uname)
ifeq ($(OS),Darwin)
  OCAMLLDFLAGS = -cclib "-framework Cocoa"
endif

include OCamlMakefile

