#!/bin/bash
total_counter=0
success_counter=0

for input in $(ls $DIR/tests/exitcodes/invalid_dataset/input.*);do
    cat $input | swipl -f none -g main -q src/main.pl 2&>1 > /dev/null
    test 3 -eq $? && ((success_counter++))
    ((total_counter++))
done

if [ $total_counter -eq $success_counter ];then
    echo -e "\e[48;5;28mAll exitcodes invalid dataset tests passed succesfully\e[0m"
else
    echo -e "\e[48;5;1mSome exitcodes invalid dataset tests failed, total score $success_counter/$total_counter\e[0m"
fi

total_counter=0
success_counter=0

for input in $(ls $DIR/tests/exitcodes/parse_error/input.*);do
    cat $input | swipl -f none -g main -q src/main.pl 2&>1 > /dev/null
    test 4 -eq $? && ((success_counter++))
    ((total_counter++))
done

if [ $total_counter -eq $success_counter ];then
    echo -e "\e[48;5;28mAll exitcodes parse error tests passed succesfully\e[0m"
else
    echo -e "\e[48;5;1mSome exitcodes parse error tests failed, total score $success_counter/$total_counter\e[0m"
fi
