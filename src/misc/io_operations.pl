:- module(io_operations, [println/1,printlines/1]).

println(X):-
    write(X),
    write("\n").

printlines([]).
printlines([L|X]):-
    println(L),
    printlines(X).
