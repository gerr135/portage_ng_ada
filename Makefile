TARGET = run_portage_ng_ada
SOURCES = src/*.ad?

# rule to link the program
portage_ng_ada: $(SOURCES)
	gprbuild -P portage_ng_ada.gpr