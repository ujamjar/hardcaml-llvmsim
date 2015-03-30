.PHONY: all install uninstall clean

all: setup.data
	ocaml setup.ml -build
	# rebuild the native code plugin so it includes the llvm libraries as well
	ocamlfind opt -shared -I _build/src -package llvm \
		llvm.cmxa llvm_analysis.cmxa llvm_target.cmxa llvm_executionengine.cmxa \
		llvm_bitreader.cmxa llvm_bitwriter.cmxa \
		_build/src/HardCamlLlvmsim.cmxa \
		_build/src/HardCamlLlvmsim.cmx \
		-o _build/src/HardCamlLlvmsim.cmxs

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
