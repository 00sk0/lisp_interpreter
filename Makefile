
default:
	ocamlbuild interpret.byte -use-menhir -cflags "-w -29 -g"
	OCAMLRUNPARAM=b rlwrap ./_build/interpret.byte

build_run:
	ocamlbuild $(file).byte -use-menhir -cflags "-w -29 -g"
	OCAMLRUNPARAM=b rlwrap ./_build/$(file).byte

clean:
	rm -r _build
	find . -name '*.byte' -delete
	find . -name '*.native' -delete
