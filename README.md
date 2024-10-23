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
bash run_test.sh test1
```

Or you can run all tests by
```
bash run_all_test.sh
```

This script will crash if one of the test failed.

### Test structure explanation

There is a function called run_testcase in lib/testUtils.ml that run the test case.

Each test case is stored in a directory (i.e. test1). Inside this, testCase.ml stores the Ast, run semant on it. If semant success, it will compile the prog in to dolphin_main.ll, and link it using clang. The script will then run the a.out executable on the input file input.txt and produce output_actual.txt. Finally, the script compares output_actual.txt and output_expected.txt.

If semant fail, output_actual.txt and output_expected.txt is the list of errors instead

## TODO list:

- Terminate unreachable code by Unreachable: Currently, an unreachable code (i.e. code after Continue and break) is still terminated by a normal branch statement.
- Test all BiNops
- Improve test22: Currently only check for error in the outermost scope, need to check the scope of inner loops as well.
