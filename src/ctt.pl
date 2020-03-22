% Module which has all functions to get/set elements of the ctt data structure
:- module(ctt, [
                new_data/1,
                get_size/3, set_size/4,
                get_turn/2, set_turn/3,
                get_orientation/3, set_orientation/4,
                get_state/2, set_state/3,
                get_tiles/2, set_tiles/3, add_tile/3
                ]).

:- use_module(tile).
:- use_module(misc).


%%% Implementation
% main datastructure: [Size:[Horizontal, Vertical], Turn, Orientation:[Horizontal, Vertical], State, Tiles:[List of all tiles]]
new_data([[0,0],"",[0,0],"",[]]).

get_size(Data,Horizontal,Vertical) :- nth0(0,Data,[Horizontal,Vertical]).
set_size(Data,Horizontal,Vertical,NewData) :- replace(Data,0,[Horizontal,Vertical],NewData).

get_turn(Data,Turn) :- nth0(1,Data,Turn).
set_turn(Data,Turn,NewData) :- replace(Data,1,Turn,NewData).

get_orientation(Data,Horizontal,Vertical) :- nth0(2,Data,[Horizontal,Vertical]).
set_orientation(Data,Horizontal,Vertical,NewData) :- replace(Data,2,[Horizontal,Vertical],NewData).

get_state(Data,State) :- nth0(3,Data,State).
set_state(Data,State,NewData) :- replace(Data,3,State,NewData).

get_tiles(Data,Tiles) :- nth0(4,Data,Tiles).
set_tiles(Data,Tiles,NewData) :- replace(Data,4,Tiles,NewData).
add_tile(Data,NewTile,NewData) :- get_tiles(Data,Tiles), append(Tiles,[NewTile],NewTiles), set_tiles(Data,NewTiles,NewData).


%%% Tests for con-tac-tic datastructure
:- begin_tests(ctt_data).
test(size):-
    new_data(Data),
    set_size(Data,10,20,NewData),
    get_size(NewData,Horizontal,Vertical),
    assertion(Horizontal == 10),
    assertion(Vertical == 20),
    !.

test(turn):-
    new_data(Data),
    set_turn(Data,"Testturn",NewData),
    get_turn(NewData,Turn),
    assertion(Turn == "Testturn"),
    !.

test(orientation):-
    new_data(Data),
    set_orientation(Data,10,20,NewData),
    get_orientation(NewData,Horizontal,Vertical),
    assertion(Horizontal == 10),
    assertion(Vertical == 20),
    !.

test(state):-
    new_data(Data),
    set_state(Data,"Teststate",NewData),
    get_state(NewData,State),
    assertion(State == "Teststate"),
    !.

test(tiles):-
    new_data(Data),
    tile:new_tile(Tile),
    set_tiles(Data,[Tile],NewData),
    get_tiles(NewData,Tiles),
    assertion(member(Tile, Tiles)),
    !.

:- end_tests(ctt_data).
