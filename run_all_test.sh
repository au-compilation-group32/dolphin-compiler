#!/bin/bash

NUM_TEST=16

for i in $(seq 1 $NUM_TEST)
do
    bash run_test.sh test$i
    if [ $? -ne 0 ]; then
        echo "test$i failed"
        exit 1
    fi
done

echo "ALL TESTS ARE SUCCESSFUL"
exit 0