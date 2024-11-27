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

## Tasks explanation

### Task 1:

Check the code in lib/ast.ml and lib/typedAst.ml

### Task 2:

Check the code in lib/srcProcessor/lexer.mll. Most feature are implemented, but unescaped string is not.

### Task 3:

Check the code in lib/srcProcessor/parser.mly.

### Task 4:

Check the code in lib/semant/semant.ml. Most positive features are implemented for string and record. Some part of Array is supported. We only have time to check for a few errors.

### Task 5:

The runtime has been incoporated properly. Check the runtime directory. Do note that we don't rename any record and function name (with the exception of main to dolphin_fun_main).

Most feature of codegen for string and record have been implemented.

### Task 6:

test 58 and 59 are new. Test 60 is for array, and it pass semant successfully.

## TODO list:

- Fix edge case of min negative integer.
- Test all BiNops
- Improve test22: Currently only check for error in the outermost scope, need to check the scope of inner loops as well.
