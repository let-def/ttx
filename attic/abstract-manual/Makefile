all: 
	dune build

clean:
	dune clean

ttx_types.ml ttx_types.mli: astgen/dsl.ml astgen/dsl_types.ml
	dune exec astgen/dsl_types.exe $@ > $@

.PHONY: all clean
