#!/bin/sh
dir=$(dirname $0)

echo "run_tests." | swipl $dir/../src/main.pl
