all:
	dune build src/opamcheck.exe src/summarize.exe

clean:
	dune clean

.PHONY: all clean
