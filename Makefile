.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe
	
test:
	OCAMLRUNPARAM=b dune exec test/main.exe
clean:
	dune clean
	rm -f adventure.zip
