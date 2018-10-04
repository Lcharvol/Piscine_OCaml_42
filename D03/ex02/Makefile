# Nom de l'executable
NAME = a.aout

# Sources
SOURCES =	cipher.ml \
			uncipher.ml
DEPS = .depend

# Compilateurs

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

# Libs

LIBS = $(WITHGRAPHICS)
WITHGRAPHICS = graphics.cma -cclib -lGraphics

# Rules

all: depend $(NAME)

$(NAME): opt byt
	ln -s $(NAME).byt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

$(NAME).byt:	$(OBJS)
		$(CAMLC) -o $(NAME).byt $(LIBS) $(OBJS)

$(NAME).opt:	$(OPTOBJS)
		$(CAMLOPT) -o $(NAME).opt $(LIBS:.cma=.cmxa) $(OPTOBJS)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

clean:
	rm -f *.cm[iox] *~ .*~ #*#
	rm -f $(NAME).o
	rm -f cipher.o
	rm -f uncipher.o

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).opt
	rm -f $(NAME).byt
	rm -f gardening.o
	rm -rf .depend

depend: $(DEPS)
$(DEPS):
	$(CAMLDEP) $(SRCC) > $(DEPS)

re: fclean all

-include .depend