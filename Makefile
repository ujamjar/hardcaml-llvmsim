.PHONY: all build clean test tag prepare publish

all: build

build:
	cp pkg/META.in pkg/META
	ocaml pkg/pkg.ml build

test:
	ocamlbuild test.otarget

clean:
	ocaml pkg/pkg.ml clean
	find . -name "*~" | xargs rm -f
	rm -f *.bc *.ll

VERSION      := $$(opam query --version)
NAME_VERSION := $$(opam query --name-version)
ARCHIVE      := $$(opam query --archive)

tag:
	git tag -a "v$(VERSION)" -m "v$(VERSION)."
	git push origin v$(VERSION)

prepare:
	opam publish prepare -r hardcaml $(NAME_VERSION) $(ARCHIVE)

publish:
	opam publish submit -r hardcaml $(NAME_VERSION)
	rm -rf $(NAME_VERSION)
