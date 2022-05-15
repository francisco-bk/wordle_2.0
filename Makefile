play:
	OCAMLRUNPARAM=b dune exec wordle2/bin/main.exe

test:
	OCAMLRUNPARAM=b dune exec wordle2/test/test.exe

zip:
	rm -f wordle2.zip
	zip -r wordle2.zip . -x@exclude.lst
	
utop:
	OCAMLRUNPARAM=b dune utop wordle2/src

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force wordle2/test/wordle2.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

docs:
	dune build @doc

doc:
	dune build @doc