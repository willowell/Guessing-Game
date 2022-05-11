.PHONY: clean
clean:
	dune clean

.PHONY: fmt-fix
fmt:
	dune build @fmt --auto-promote

.PHONY: run
run:
	dune exec ./bin/main.exe

.PHONY: test
test:
	dune runtest