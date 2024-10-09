# dolphin-compiler

## Dependencies:

```
opam install printbox printbox-text
```

## Run test:

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

Each test case is stored in a directory (i.e. test1). Inside this, testCase.ml stores the Ast, run semant on it. If semant success, it will compile the prog in to dolphin_main.ll, and link it using clang. The script will then run the a.out executable on the input file input.txt and produce output_actual.txt. Finally, the script compares output_actual.txt and output_expected.txt.

If semant fail, output_actual.txt and output_expected.txt is the list of errors instead

## Question 1

A rule of thumb is to use infertype when the type is not provided by the AST, for example VarDecl without a type, and use typecheck whenever the AST explicitly tell us about the type.

## Question 2

We try to avoid ErrorType as much as possible, by choose the most suitable type. For example, binop type will be infered from its operator even when the operand type are invalid. For assignment, if there is disagreement between the lval and expr, we prioritize lval type, if both types are not available, we have to put an ErrorType in it.

## Question 3 (glory)

See implementation in code. test5 tests short-circuit feature extensively.
