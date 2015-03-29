.PHONY: all install uninstall clean

all: setup.data
	ocaml setup.ml -build

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure 

doc: setup.ml
	ocaml setup.ml -doc

install: all
	ocaml setup.ml -install

uninstall: 
	ocamlfind remove hardcaml-llvmsim

clean:
	ocaml setup.ml -clean
	find . -name "*~" | xargs rm -f
	rm -f *.bc *.ll

distclean:
	ocaml setup.ml -distclean
