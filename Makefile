##############################
#
# Mattricks
#
# Compilation: 
# Option 1: Simply type "make <phase>"(phase can be 'parse' or 'scanner') to compile the Mattricks. Will auto run example.mc test
# Option 2: "ocamlbuild <phase>.native" will also build the Mattricks

# For testing, you can run the binary executable and test it with example.mc, it will generate example.out
# Or use ./test [optioanl: flags -cpsaf:] to test with test suite
##############################

parse : test_parse.out

test_parse : parse.mly ast.ml scanner.mll test_parse.ml
	ocamlbuild test_parse.native

test_parse.out : test_parse example.mc
	./test_parse.native < example.mc > example.out

##############################

semant : test_semant.out

test_semant : parse.mly ast.ml scanner.mll sast.ml semant.ml test_semant.ml
	ocamlbuild test_semant.native

test_semant.out : test_semant example.mc
	./test_semant.native < example.mc > example.out

compiler :
	ocamlbuild -pkgs llvm microc.native
	./microc.native < example.mc > example.out
	lli example.out
##############################

clean:
	rm -rf *.out *.native /test_cases/parse/temp /test_cases/semant/temp _build/

