##############################
#
# Mattricks
#
# Compilation: 
# Option 1: Simply type "make <phase>"(phase can be 'parse' or 'scanner' or 'compiler') to compile the Mattricks. Will auto run example.mc test
# Option 2: "ocamlbuild <phase>.native" will also build the Mattricks
#
# For testing, you can run the binary executable and test it with example.mc, it will generate example.out
# Or simply call "make test_all" to test with test suites
# Or use ./test_script [optioanl: flags -cpsaf:] to test with test suite
##############################

test_all :
	./test_script

##############################

test_parse : parse.mly ast.ml scanner.mll ./test_cases/test_parse.ml
	cp ./test_cases/test_parse.ml ./
	ocamlbuild test_parse.native
	rm ./test_parse.ml

parse : test_parse example.mc
	./test_parse.native < example.mc > example.out

##############################

test_semant : parse.mly ast.ml scanner.mll sast.ml semant.ml ./test_cases/test_semant.ml
	cp ./test_cases/test_semant.ml ./
	ocamlbuild test_semant.native
	rm ./test_semant.ml

semant : test_semant example.mc
	./test_semant.native < example.mc > example.out

##############################

compiler :
	ocamlbuild -pkgs llvm microc.native
	./microc.native < example.mc > example.out
	lli example.out

##############################

clean:
	rm -rf *.out *.native /test_cases/parse/temp /test_cases/semant/temp _build/

