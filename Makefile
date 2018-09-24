all:
	dune build src/opamcheck.exe

clean:
	dune clean

.PHONY: all clean
