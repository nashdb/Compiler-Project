OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=                 # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

# compile should be compiled to bytecode, and is composed of three
# units: mod1, mod2 and mod3.

# The list of object files for prog1
PROG1_OBJS=util.cmo symbol.cmo environment.cmo sets.cmo typ.cmo ast.cmo \
		label.cmo mips.cmo bases.cmo lexer.cmo debug.cmo parser.cmo static.cmo \
		quads.cmo control.cmo name.cmo lift.cmo copyprop.cmo codegen.cmo compile.cmo

mc:	$(PROG1_OBJS)
	$(OCAMLC) -o mc $(OCAMLFLAGS) unix.cma $(PROG1_OBJS)

# prog2 should be compiled to native-code, and is composed of two
# units: mod4 and mod5.

# The list of object files for prog2
#PROG2_OBJS=mod4.cmx mod5.cmx

#prog2:	$(PROG2_OBJS)
#	$(OCAMLOPT) -o prog2 $(OCAMLFLAGS) $(PROG2_OBJS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f *.cm[iox]

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
