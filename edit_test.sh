#!/bin/bash

NUM_TEST=42

for i in $(seq 1 $NUM_TEST)
do
    file_name=test/test$i/main.dlp
    { echo "int main() {"; cat $file_name;echo ""; echo "}"; } > temp && mv temp $file_name
    if [ $? -ne 0 ]; then
        echo "test$i failed"
        exit 1
    fi
done

echo "ALL TESTS ARE SUCCESSFUL"
exit 0
