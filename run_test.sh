#!/bin/bash

export OCAMLRUNPARAM=b

test_name=$1

# Cleaning
rm $test_name/output_actual.txt 2> /dev/null
rm $test_name/dolphin_main.ll 2> /dev/null

# Run semantic analysis
dune exec bin/runTest.exe $test_name

# Check result of semantic analysis
if [ $? -ne 0 ]; then
    echo "Semantics analysis failed, comparing error lists:"
    diff $test_name/output_actual.txt $test_name/output_expected.txt
    if [ $? -ne 0 ]; then
        echo "diff reports that expected and actual output are different"
        exit 1
    else
        echo "diff reports that expected and actual output are the same"
        exit 0
    fi
else
    echo "Semantics analysis success, compile and run"
    clang runtime/stdlib.c runtime/runtime.c runtime/runtime.h $test_name/dolphin_main.ll
    ./a.out < $test_name/input.txt > $test_name/output_actual.txt 2>&1
    echo "return $?" >> $test_name/output_actual.txt
    diff $test_name/output_actual.txt $test_name/output_expected.txt
    if [ $? -ne 0 ]; then
        echo "diff reports that expected and actual output are different"
        rm a.out
        exit 1
    else
        echo "diff reports that expected and actual output are the same"
        rm a.out
        exit 0
    fi
fi
