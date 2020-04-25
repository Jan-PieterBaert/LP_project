#!/bin/bash
winable_inputs=$(ls $DIR/tests/winable/input.*)
winable_outputs=$(ls $DIR/tests/winable/output.*)

total_counter=0
success_counter=0
while read input <&3 && read output <&4; do
   temp_out=$(mktemp -p /tmp test_winable_XXXXX)
   cat $input | swipl -f none -g main -q src/main.pl > $temp_out
   diff $output $temp_out -q && ((success_counter++))
   ((total_counter++))
   rm $temp_out
done 3<<<"$winable_inputs" 4<<<"$winable_outputs"

if [ $total_counter -eq $success_counter ];then
    echo -e "\e[48;5;28mAll winable tests passed succesfully\e[0m"
else
    echo -e "\e[48;5;1mSome winable tests failed, total score $success_counter/$total_counter\e[0m"
fi
