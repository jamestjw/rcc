#!/bin/sh

# This script goes through the test files in the TEST_DIR to figure out the last
# index and creates new test files with a new index.

TEST_DIR="tests/input/"

regex='input([0-9]+)'
max_idx=0

for i in $(ls $TEST_DIR | grep 'input[0-9]\+-\(err\|ok\).c')
do
    [[ $i =~ $regex ]]
    curr_idx=10#${BASH_REMATCH[1]}

    if [[ $curr_idx -gt $max_idx ]]
    then
        max_idx=$curr_idx
    fi
done

echo "Last test file had index $max_idx"

new_idx=$(($max_idx + 1));
new_idx=$(printf "%03d" $new_idx)

if [ -z "$1" ]
then
    new_test_status="ok"
else
    new_test_status=$1
fi
new_test_file_name=$TEST_DIR"input$new_idx-$new_test_status.c"
new_test_res_name=$TEST_DIR"input$new_idx.exp"

echo "Creating new test files $new_test_file_name and $new_test_res_name"
touch $new_test_file_name
touch $new_test_res_name