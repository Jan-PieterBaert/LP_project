#!/bin/bash
not_winable_inputs=$(ls $DIR/tests/not_winable/input.*)

total_counter=0
success_counter=0
while read input <&3 ; do
   cat $input | swipl -f none -g main -q src/main.pl > /dev/null && ((success_counter++))
   ((total_counter++))
done 3<<<"$not_winable_inputs"

if [ $total_counter -eq $success_counter ];then
    echo -e "\e[48;5;28mAll not_winable tests passed succesfully\e[0m"
else
    echo -e "\e[48;5;1mSome not_winable tests failed, total score $success_counter/$total_counter\e[0m"
fi
