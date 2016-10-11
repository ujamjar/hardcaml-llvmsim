.PHONY: all build clean

all: build

build:
	ocaml pkg/pkg.ml build

clean:
	ocaml pkg/pkg.ml clean
	find . -name "*~" | xargs rm -f
	rm -f *.bc *.ll

