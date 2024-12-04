# dolphin-compiler

## Dependencies:

```
opam install printbox printbox-text
```

## Run test:

Run a single test case with:

```
bash run_test.sh <test_name>
```

For example

```
bash run_test.sh test/test1
```

Or you can run all tests by
```
bash run_all_test.sh
```

This script runs all the tests without any output. It will crash if one of the test failed. If you want more output (i.e. Ast printing, error list printing, ...), please run single test case.

I also provide ``compile_prog`` in ``bin/compile.ml``, you can run it with

```
dune exec bin/compile.exe test/test1/main.dlp
```

### Test structure explanation

There is a function called run_testcase in lib/testUtils.ml that run the test case.

Each test case is stored in a directory (i.e. test1). Inside this, main.dlp stores the source code, run parsing and semant on it. If semant success, it will compile the prog in to dolphin_main.ll, and link it using clang. The script will then run the a.out executable on the input file input.txt and produce output_actual.txt. Finally, the script compares output_actual.txt and output_expected.txt.

If semant fail, output_actual.txt and output_expected.txt is the list of errors instead

##

List of fixes:

### Phase 1:

all issues has been fixed

### Phase 2:

all issues has been fixed

### Phase 3:

all issues has been fixed with the exception of the shift/reduce conflict involving ELSE

### Phase 4:

The shift/reduce conflict is the same as phase 3, so I'll consider that all issues has been fixed.

### Phase 5:

Everything has been implemented. The only issues we have is the default value of string doesn't work.

Another thing is that we choose not to implement Nil as an Undetermined type, but create a typ in TypedAst call Nil. This Nil type receive special treatment similar to TAst.Void and TAst.ErrorType.
This approach does allowed us to infer the type of the Nil expr, but otherwise, it works.

## Phase 5 Tasks explanation

### Task 1:

Check the code in lib/ast.ml and lib/typedAst.ml

### Task 2:

Check the code in lib/srcProcessor/lexer.mll. Most feature are implemented, including unescaped strings.

### Task 3:

Check the code in lib/srcProcessor/parser.mly.

### Task 4:

Check the code in lib/semant/semant.ml.

### Task 5:

The runtime has been incoporated properly. Check the runtime directory. Do note that we don't rename any record and function name (with the exception of main to dolphin_fun_main).

### Task 6:

test 58-75 are new. Test 80 fails since we have seg fault when implementing default string.
