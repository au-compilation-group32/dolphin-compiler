#!/test/bash

test_name=$1

dune exec bin/runTest.exe $test_name

if [ $? -ne 0 ]; then
    echo "Semantics analysis failed, comparing error lists:"
    diff test/$test_name/output_actual.txt test/$test_name/output_expected.txt
    if [ $? -ne 0 ]; then
        echo "diff reports that expected and actual output are different"
        exit 1
    else
        echo "diff reports that expected and actual output are the same"
        exit 0
    fi
else
    echo "Semantics analysis success, compile and run"
    clang main.c test/$test_name/dolphin_main.ll
    ./a.out < test/$test_name/input.txt > test/$test_name/output_actual.txt
    diff test/$test_name/output_actual.txt test/$test_name/output_expected.txt
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
