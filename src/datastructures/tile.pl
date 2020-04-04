% Module which has all functions to get/set elements of the tile data structure
:- module(tile, [
                new_tile/4,
                get_tile_coord/3,set_tile_coord/4,
                get_tile_color/2,set_tile_color/3,

                get_neigh_tiles/3
    ]).

% tile datastructure: [Coord: X/Y, Color]
new_tile([X/Y,Color],X,Y,Color).
get_tile_coord([X/Y, _],X,Y).
set_tile_coord([_,C],X,Y,[X/Y,C]).

get_tile_color([_,Color],Color).
set_tile_color([C,_],NewColor,[C,NewColor]).

get_neigh_tiles(_,[],[]).
get_neigh_tiles(Tile,[TestTile|AllTiles],[TestTile|Neighs]) :-
    get_tile_coord(Tile,T1_X,T1_Y),
    get_tile_coord(TestTile,T2_X,T2_Y),
    T3_X is T1_X - 1,
    T4_X is T1_X + 1,
    T3_Y is T1_Y - 1,
    T4_Y is T1_Y + 1,
    member(T2_X/T2_Y, [T3_X/T1_Y, T3_X/T4_Y, T1_X/T3_Y, T4_X/T3_Y, T4_X/T1_Y, T4_X/T4_Y]),
    get_neigh_tiles(Tile,AllTiles,Neighs),
    !.

get_neigh_tiles(Tile,[_|AllTiles],Neighs) :-
    get_neigh_tiles(Tile,AllTiles,Neighs).


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
    new_tile(Tile,2,2,"Tile"),
    new_tile(T0,1,1,"T0"),
    new_tile(T1,1,2,"T1"),
    new_tile(T2,1,3,"T2"),
    new_tile(T3,1,4,"T3"),
    new_tile(T4,2,0,"T4"),
    new_tile(T5,2,1,"T5"),
    new_tile(T6,2,2,"T6"),
    new_tile(T7,2,3,"T7"),
    new_tile(T8,2,4,"T8"),
    new_tile(T9,3,1,"T9"),
    new_tile(T10,3,2,"T10"),
    new_tile(T11,3,3,"T11"),
    new_tile(T12,3,4,"T12"),
    get_neigh_tiles(Tile,[T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12],Result),
    assertion(Result == [T1,T2,T5,T9,T10,T11]),
    !.

:- end_tests(tile).
