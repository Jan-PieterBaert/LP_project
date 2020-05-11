% Module which has all functions to get/set elements of the tile data structure
:- module(tile, [
                new_tile/4,
                get_tile_coord/3, set_tile_coord/4,
                get_tile_color/2, set_tile_color/3,

                get_neigh_coords/3, get_neigh_coords_from_list/3,
                print_tile/1
    ]).
% :- table is_neight_coord/2.

% tile datastructure: [Coord: X/Y, Color]
new_tile(tile(X/Y, Color), X, Y, Color).
get_tile_coord(tile(X/Y, _), X, Y).
set_tile_coord(tile(_, C), X, Y, tile(X/Y, C)).

get_tile_color(tile(_, Color), Color).
set_tile_color(tile(C, _), New_color, tile(C, New_color)).

print_tile(tile(X/Y, Color)) :-
    char_code("A", A), N is X+A, char_code(L, N), Y1 is Y+1,
    write("    ("), write(L), write(Y1), write(") -> "), write(Color), write("\n").

is_neigh_coord(X/Y, Test_coord) :-
    Minus_one_X is X - 1,
    Plus_one_X  is X + 1,
    Minus_one_Y is Y - 1,
    Plus_one_Y  is Y + 1,
    member(Test_coord, [Minus_one_X/Y, Minus_one_X/Plus_one_Y, X/Minus_one_Y, Plus_one_X/Minus_one_Y, Plus_one_X/Y, X/Plus_one_Y]).

% get_neigh_coords(Coord, All_coords, Neighs) :-
%     findall(C, (member(C, All_coords), is_neigh_coord(Coord, C)), Neighs).

get_neigh_coords(_, [], []).
get_neigh_coords(Coord, [Test_coord|All_coords], [Test_coord|Neighs]) :-
    is_neigh_coord(Coord, Test_coord),
    get_neigh_coords(Coord, All_coords, Neighs),
    !.

get_neigh_coords(Coord, [_|All_coords], Neighs) :-
    get_neigh_coords(Coord, All_coords, Neighs).

% get_neigh_coords_from_list(Coords, All_coords, Neighs) :-
%     setof(N, (member(C, Coords), is_neigh_coord(C, N), member(N, All_coords)), Neighs).

get_neigh_coords_from_list([], _, []).
get_neigh_coords_from_list([Coord|Coords], All_coords, Neighs) :-
    get_neigh_coords(Coord, All_coords, Result1),
    remove_elements(All_coords, Result1, New_all_coords),
    get_neigh_coords_from_list(Coords, New_all_coords, Result2),
    append(Result1, Result2, Neighs).

:- begin_tests(tile).
test(coord) :-
    new_tile(Tile, 0, 0, ""),
    set_tile_coord(Tile, 10, 20, New_tile),
    get_tile_coord(New_tile, X, Y),
    assertion(X == 10),
    assertion(Y == 20),
    !.

test(color) :-
    new_tile(Tile, 0, 0, ""),
    set_tile_color(Tile, "Testcolor", New_tile),
    get_tile_color(New_tile, Color),
    assertion(Color == "Testcolor"),
    !.

test(neighborhood) :-
    Tile=2/2,
    T0=1/1,
    T1=1/2,
    T2=1/3,
    T3=1/4,
    T4=2/0,
    T5=2/1,
    T6=2/2,
    T7=2/3,
    T8=2/4,
    T9=3/1,
    T10=3/2,
    T11=3/3,
    T12=3/4,
    get_neigh_coords(Tile, [T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], Result),
    assertion(Result == [T1, T2, T5, T7, T9, T10]),
    !.

:- end_tests(tile).
