:- module(list_operations, [replace/4]).

% replace(+List, +Index, +Value, -NewList).
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, I1 is I-1, replace(T, I1, X, R).
