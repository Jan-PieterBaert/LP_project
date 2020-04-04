:- module(solve,[
        get_all_states/2,
        get_best_state/2
    ]).

generate_tiles(0,_,[],_,0):-!.
generate_tiles(X,Y,[Tile|Tiles],Col,0):-
    X1 is X-1,
    new_tile(Tile,X1,Y,Col),
    generate_tiles(X1,Y,Tiles,Col,0).

generate_tiles(_,0,[],_,1):-!.
generate_tiles(X,Y,[Tile|Tiles],Col,1):-
    Y1 is Y-1,
    new_tile(Tile,X,Y1,Col),
    generate_tiles(X,Y1,Tiles,Col,1).

get_new_tiles(Data,0,Tiles):-
    get_orientation(Data,Col,_),
    get_size(Data,Max,_),
    generate_tiles(Max,-1,NewTiles1,Col,0),
    generate_tiles(Max,Max,NewTiles2,Col,0),
    append(NewTiles1,NewTiles2,NewTiles),
    write(NewTiles),write("-\n"),write(Tiles),write("-\n").

get_new_tiles(Data,1,Tiles):-
    get_orientation(Data,Col,_),
    get_size(Data,Max,_),
    generate_tiles(-1,Max,NewTiles1,Col,1),
    generate_tiles(Max,Max,NewTiles2,Col,1),
    append(NewTiles1,NewTiles2,NewTiles),
    write(NewTiles),write("-\n"),write(Tiles),write("-\n").

state_direction(Data,0):-
    get_turn(Data,Turn),
    get_orientation(Data,Turn,_).

state_direction(Data,1):-
    get_turn(Data,Turn),
    get_orientation(Data,_,Turn).

get_all_states(Data,States):-
    get_turn(Data,Turn),
    state_direction(Data,Direction),
    get_tiles(Data,Tiles),
    get_new_tiles(Data,Direction,Tiles),
    write(Direction), write("\n"),
    !.

get_best_state(Data,State):- fail.
