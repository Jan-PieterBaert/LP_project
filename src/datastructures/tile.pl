% Module which has all functions to get/set elements of the tile data structure
:- module(tile, [
                new_tile/4,
                get_tile_coord/3,set_tile_coord/4,
                get_tile_color/2,set_tile_color/3,

                get_neigh_coords/3, get_neigh_coords_from_list/3,
                print_tile/1
    ]).

% tile datastructure: [Coord: X/Y, Color]
new_tile([X/Y,Color],X,Y,Color).
get_tile_coord([X/Y, _],X,Y).
set_tile_coord([_,C],X,Y,[X/Y,C]).

get_tile_color([_,Color],Color).
set_tile_color([C,_],NewColor,[C,NewColor]).

get_neigh_coords(_,[],[]).
get_neigh_coords(Coord,[TestCoord|AllCoords],[TestCoord|Neighs]) :-
    Coord = T1_X/T1_Y,
    TestCoord = T2_X/T2_Y,
    T3_X is T1_X - 1,
    T4_X is T1_X + 1,
    T3_Y is T1_Y - 1,
    T4_Y is T1_Y + 1,
    member(T2_X/T2_Y, [T3_X/T1_Y, T3_X/T4_Y, T1_X/T3_Y, T4_X/T3_Y, T4_X/T1_Y, T4_X/T4_Y]),
    get_neigh_coords(Coord,AllCoords,Neighs),
    !.

get_neigh_coords(Coord,[_|AllCoords],Neighs) :-
    get_neigh_coords(Coord,AllCoords,Neighs).

get_neigh_coords_from_list([],_,[]).
get_neigh_coords_from_list([Coord|Coords], AllCoords, Neighs):-
    get_neigh_coords(Coord,AllCoords,Result1),
    get_neigh_coords_from_list(Coords,AllCoords,Result2),
    append(Result1,Result2,Neighs).

print_tile([X/Y,Color]) :-
    char_code("A",A), N is X+A, atom_codes(L,[N]), Y1 is Y+1,
    write("    ("), write(L), write(Y1), write(") -> "), write(Color), write("\n").

:- begin_tests(tile).
test(coord):-
    new_tile(Tile,0,0,""),
    set_tile_coord(Tile,10,20,NewTile),
    get_tile_coord(NewTile,X,Y),
    assertion(X == 10),
    assertion(Y == 20),
    !.

test(color):-
    new_tile(Tile,0,0,""),
    set_tile_color(Tile,"Testcolor",NewTile),
    get_tile_color(NewTile,Color),
    assertion(Color == "Testcolor"),
    !.

test(neighborhood):-
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
    get_neigh_coords(Tile,[T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12],Result),
    assertion(Result == [T1,T2,T5,T9,T10,T11]),
    !.

:- end_tests(tile).
