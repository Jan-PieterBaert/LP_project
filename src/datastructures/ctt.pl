% Module which has all functions to get/set elements of the ctt data structure
:- module(ctt, [
                new_data/1, new_data/6,
                get_size/3, set_size/4,
                get_turn/2, set_turn/3,
                get_orientation/3, set_orientation/4,
                get_state/2, set_state/3,
                get_tiles/2, set_tiles/3, add_tile/3,
                print_data/1
                ]).

:- use_module(tile).


%%% Implementation
% main datastructure: [Size:X/Y, Turn, Orientation:X/Y, State, Tiles:[List of all tiles]]
new_data([0/0,"",0/0,"",[]]).
new_data([SizeX/SizeY,Turn,OriX/OriY,State,Tiles],(SizeX,SizeY),Turn,(OriX,OriY),State,Tiles).

get_size([X/Y,_,_,_,_],X,Y).
set_size([_,A,B,C,D],X,Y,[X/Y,A,B,C,D]).

get_turn([_,Turn,_,_,_],Turn).
set_turn([A,_,B,C,D],Turn,[A,Turn,B,C,D]).

get_orientation([_,_,X/Y,_,_],X,Y).
set_orientation([A,B,_,C,D],X,Y,[A,B,X/Y,C,D]).

get_state([_,_,_,State,_],State).
set_state([A,B,C,_,D],State,[A,B,C,State,D]).

get_tiles([_,_,_,_,Tiles],Tiles).
set_tiles([A,B,C,D,_],Tiles,[A,B,C,D,Tiles]).
% Will prepend a tile to the list of tiles
add_tile([A,B,C,D,Tiles],NewTile,[A,B,C,D,[NewTile|Tiles]]).

print_data([SizeX/SizeY,Turn,OriX/OriY,State,Tiles]):-
    length(Tiles,L),
    write("tiles: "), write(L), write("\n"),
    maplist(print_tile, Tiles),
    write("state: "), write(State), write("\n"),
    write("orientation: "), write(OriX), write(" * "), write(OriY), write("\n"),
    write("size: "), write(SizeX), write(" * "), write(SizeY), write("\n"),
    write("turn: "), write(Turn), write("\n").

%%% Tests for con-tac-tic datastructure
:- begin_tests(ctt_data).
test(size):-
    new_data(Data),
    set_size(Data,10,20,NewData),
    get_size(NewData,X,Y),
    assertion(X == 10),
    assertion(Y == 20),
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
    get_orientation(NewData,X,Y),
    assertion(X == 10),
    assertion(Y == 20),
    !.

test(state):-
    new_data(Data),
    set_state(Data,"Teststate",NewData),
    get_state(NewData,State),
    assertion(State == "Teststate"),
    !.

test(tiles):-
    new_data(Data0),
    new_tile(T1,0,0,""),
    new_tile(T2,1,1,""),
    new_tile(T3,2,2,""),
    set_tiles(Data0,[T1,T2],Data1),
    get_tiles(Data1,Tiles1),
    assertion(Tiles1 == [T1,T2]),
    add_tile(Data1,T3,Data2),
    get_tiles(Data2,Tiles2),
    assertion(Tiles2 == [T3,T1,T2]),
    !.

:- end_tests(ctt_data).
