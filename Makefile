
default:
	ocamlbuild eval.byte -use-menhir -cflags "-w -29 -g"
	OCAMLRUNPARAM=b rlwrap ./_build/eval.byte

build_run:
	ocamlbuild $(file).byte -use-menhir -cflags "-w -29 -g"
	OCAMLRUNPARAM=b rlwrap ./_build/$(file).byte


