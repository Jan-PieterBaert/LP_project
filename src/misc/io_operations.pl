:- module(io_operations, [
        println/1,
        printlines/1,
        exit_with_code_and_message/2,
        write_debug/1
    ]).

println(X) :-
    write(X),
    write("\n").

printlines([]).
printlines([L|X]) :-
    println(L),
    printlines(X).

exit_with_code_and_message(Code, Error_message) :-
    set_output(user_error),
    println(Error_message),
    halt(Code).


% When the following line is commented, debug statements will be printed
write_debug(_) :- !.

write_debug([]).
write_debug([H|T]) :-
    write(H), write_debug(T).
