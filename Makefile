OCAMLC=/opt/local/bin/ocamlc
OCAMLOPT=/opt/local/bin/ocamlopt
OCAMLDEP=/opt/local/bin/ocamldep
INCLUDES=-I +glMLite GL.cma Glu.cma Glut.cma # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here
LD_LIBRARY_PATH=/opt/local/lib/ocaml/glMLite/
PAPERPLANES_OBJS=Geometry.cmo PaperPlanes.cmo

paperplanes: $(PAPERPLANES_OBJS)
	$(OCAMLC) -o PaperPlanes $(OCAMLFLAGS) $(PAPERPLANES_OBJS) -dllpath ${LD_LIBRARY_PATH}

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
	rm -f PaperPlanes
	rm -f *.cm[iox]

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
