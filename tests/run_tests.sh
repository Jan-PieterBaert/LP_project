#!/bin/bash
export DIR=$(dirname $(dirname $(readlink -f $0)))

for file in $(ls $DIR/tests/run_*_tests.sh);do
    echo -e "\e[48;5;21mTesting file $file\e[0m"
    $file
    echo "-------------------------------------"
    echo
done
