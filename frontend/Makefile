fmt:
	@dune build --auto-promote @fmt

build: fmt
	@dune build

run:
	@dune exec magizhchi

clean:
	@dune clean

WATCH ?= @all
watch:
	@dune build $(WATCH) -w
