RESULT = test
SOURCES = filemonoid.ml try.ml tama.ml main.ml
LIBS = bigarray sdl sdlloader sdlttf sdlmixer
INCDIRS = +sdl ~/.brew/lib
OCAMLLDFLAGS = -cclib "-framework Cocoa"
THREADS = true
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)

install:
	brew install ocamlsdl