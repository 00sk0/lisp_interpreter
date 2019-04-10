default:
	ocamlbuild interpret.byte -use-menhir -cflags "-w -29 -g"
	OCAMLRUNPARAM=b rlwrap ./_build/interpret.byte

clean:
	find . -name '*.byte' -delete
	find . -name '*.native' -delete
