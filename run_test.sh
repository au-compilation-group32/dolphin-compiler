#!/bin/bash

test_name=$1

dune exec bin/$test_name/testCase.exe

if [ $? -ne 0 ]; then
    echo "Semantics analysis failed, comparing error lists:"
    diff bin/$test_name/output_actual.txt bin/$test_name/output_expected.txt
    if [ $? -ne 0 ]; then
        echo "diff reports that expected and actual output are different"
    else
        echo "diff reports that expected and actual output are the same"
    fi
else
    echo "Semantics analysis success, compile and run"
    clang main.c bin/$test_name/dolphin_main.ll
    ./a.out < bin/$test_name/input.txt > bin/$test_name/output_actual.txt
    diff bin/$test_name/output_actual.txt bin/$test_name/output_expected.txt
    if [ $? -ne 0 ]; then
        echo "diff reports that expected and actual output are different"
    else
        echo "diff reports that expected and actual output are the same"
    fi
    rm a.out
fi