:- module(list_operations, [generate_pairs/3, generate_to_from/3, remove_elements/3]).

% Generate all possible pairs of elements in List1 and List2
generate_pairs(List1, List2, Pairs) :-
    findall((X/Y), (member(X, List1), member(Y, List2)), Pairs).

% generate a list from X to Y, e.g. if X=0 and Y=3, the return value will be [0, 1, 2]
generate_to_from(X, X, []) :- !.
generate_to_from(X, Y, [X|Retval]) :-
    X1 is X+1,
    generate_to_from(X1, Y, Retval).

% Remove the elements of L that are in [H|T]
remove_elements(L, [], L).
remove_elements(L, [H|T], R) :-
    delete(L, H, R1),
    remove_elements(R1, T, R).
