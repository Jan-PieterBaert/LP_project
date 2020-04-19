:- module(io_operations, [
        println/1,
        printlines/1,
        write_debug/1
    ]).

println(X) :-
    write(X),
    write("\n").

printlines([]).
printlines([L|X]) :-
    println(L),
    printlines(X).

% When the following line is commented, debug statements will be printed
write_debug(_) :- !.

write_debug([]).
write_debug([H|T]) :-
    write(H), write_debug(T).
