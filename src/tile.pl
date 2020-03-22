% Module which has all functions to get/set elements of the tile data structure
:- module(tile, [
                new_tile/1,
                get_tile_coord/3,set_tile_coord/4,
                get_tile_color/2,set_tile_color/3
    ]).

:- use_module(misc).

% tile datastructure: [Coord: [Horizontal, Vertical], Color]
new_tile([[0,0], ""]).
get_tile_coord(Tile,Horizontal,Vertical) :- nth0(0,Tile,[Horizontal,Vertical]).
set_tile_coord(Tile,Horizontal,Vertical,NewTile) :- replace(Tile,0,[Horizontal,Vertical],NewTile).

get_tile_color(Tile,Color):- nth0(1,Tile,Color).
set_tile_color(Tile,NewColor,NewTile):- replace(Tile,1,NewColor,NewTile).

:- begin_tests(tile).
test(coord):-
    new_tile(Tile),
    set_tile_coord(Tile,10,20,NewTile),
    get_tile_coord(NewTile,Horizontal,Vertical),
    assertion(Horizontal == 10),
    assertion(Vertical == 20),
    !.

test(color):-
    new_tile(Tile),
    set_tile_color(Tile,"Testcolor",NewTile),
    get_tile_color(NewTile,Color),
    assertion(Color == "Testcolor"),
    !.

:- end_tests(tile).
