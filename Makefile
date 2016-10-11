.PHONY: all build clean test

all: build

build:
	ocaml pkg/pkg.ml build

test:
	ocamlbuild test.otarget

clean:
	ocaml pkg/pkg.ml clean
	find . -name "*~" | xargs rm -f
	rm -f *.bc *.ll

