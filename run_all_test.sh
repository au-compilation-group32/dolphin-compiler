#!/bin/bash

NUM_TEST=59

for i in $(seq 1 $NUM_TEST)
do
    bash run_test.sh test/test$i >/dev/null 2>/dev/null
    if [ $? -ne 0 ]; then
        echo "test$i failed"
        exit 1
    else
        echo "test$i succeeded"
    fi
done

echo "ALL TESTS ARE SUCCESSFUL"
exit 0
