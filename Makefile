default: native

byte:
	ocamlbuild interpret.byte -use-menhir -cflags "-w -29 -g"
	OCAMLRUNPARAM=b rlwrap ./_build/interpret.byte

native:
	ocamlbuild interpret.native -use-menhir -cflags "-w -29 -g"
	OCAMLRUNPARAM=b rlwrap ./_build/interpret.native

clean:
	find . -name '*.byte' -delete
	find . -name '*.native' -delete
