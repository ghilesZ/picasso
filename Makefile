build:
	dune build

test:
	@dune runtest -f

clean:
	dune clean

install:
	dune install picasso

uninstall:
	dune uninstall picasso

.PHONY: build test clean
