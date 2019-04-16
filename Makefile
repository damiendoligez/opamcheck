all:
	dune build src/opamcheck.exe

debug:
	dune build src/opamcheck.bc

clean:
	dune clean

.PHONY: all clean
