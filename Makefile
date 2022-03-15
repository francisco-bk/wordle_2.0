test:
	OCAMLRUNPARAM=b dune exec wordle2/test/wordle2.exe

zip:
	rm -f wordle2.zip
	zip -r wordle2.zip . -x@exclude.lst