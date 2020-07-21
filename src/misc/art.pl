:- module(art_printing,[
        print_board_art/1
    ]).

print_empty_tile             :- write(" \u2B21").
print_colored_tile("red")    :- write(" \x1b[31m\u2B22\x1b[39;49m").
print_colored_tile("green")  :- write(" \x1b[32m\u2B22\x1b[39;49m").
print_colored_tile("yellow") :- write(" \x1b[33m\u2B22\x1b[39;49m").
print_colored_tile("blue")   :- write(" \x1b[34m\u2B22\x1b[39;49m").
print_colored_tile("magenta"):- write(" \x1b[35m\u2B22\x1b[39;49m").
print_colored_tile("cyan")   :- write(" \x1b[36m\u2B22\x1b[39;49m").
print_colored_tile("white")  :- write(" \x1b[97m\u2B22\x1b[39;49m").
print_colored_tile(_)        :- write(" \x1b[92m\u2B22\x1b[39;49m").

tile_to_tuple(Tile, (Y, X, Color)) :-
    get_tile_color(Tile, Color),
    get_tile_coord(Tile, X, Y).

print_board_art(Board) :-
    get_size(Board, Y, X),
    print_letters(0,Y),
    get_tiles(Board, Tiles),
    maplist(tile_to_tuple, Tiles, Tuples),
    sort(Tuples, Sorted_tuples),
    print_rows(0, X, Y, Sorted_tuples).

print_letters(Y,Y) :- write("\n").
print_letters(X,Y) :-
    char_code("A", A), N is X+A, char_code(L, N),
    write(" "), write(L),
    X1 is X + 1, print_letters(X1, Y).

write_spaces(0).
write_spaces(X) :- write(" "), X1 is X-1, write_spaces(X1).

print_rows(X, X, _, _).
print_rows(A, X, YMax, Tuples) :-
    A1 is A+1, write_spaces(A), write(A1), % write all tuples from this row
    print_art_tuples(0, A, YMax, Tuples, New_tuples),
    write("\n"),
    print_rows(A1, X, YMax, New_tuples).

print_art_tuples(Y, _, Y, Tuples, Tuples).
print_art_tuples(Y, X, YMax, [(X,Y,Color)|Tuples], New_tuples) :-
    string_lower(Color, Color_lower),
    print_colored_tile(Color_lower),
    Y1 is Y + 1,
    print_art_tuples(Y1, X, YMax, Tuples, New_tuples).

print_art_tuples(Y, X, YMax, Tuples, New_tuples) :-
    print_empty_tile,
    Y1 is Y + 1,
    print_art_tuples(Y1, X, YMax, Tuples, New_tuples).

