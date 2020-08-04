all: bignum bignum_tests

bignum: bignum.ml
	ocamlbuild -use-ocamlfind bignum.byte

bignum_tests: bignum_tests.ml
	ocamlbuild -use-ocamlfind bignum_tests.byte

clean:
	rm -rf _build *.byte
