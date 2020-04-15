:- module(solve,[
        get_all_states/2,
        get_best_state/2
    ]).

:- use_module(misc/list_operations).

% Generate the extra coords for the fields outside the border, where the color starts
generate_coords(0,_,[],0):-!.
generate_coords(X,Y,[X/Y|Coords],0):-
    X1 is X-1,
    generate_coords(X1,Y,Coords,0).

generate_coords(_,0,[],1):-!.
generate_coords(X,Y,[X/Y|Coords],1):-
    Y1 is Y-1,
    generate_coords(X,Y1,Coords,1).

% Get the extra coords for the fields outside the border, where the color starts
get_new_coords(Data,0,NewCoords):-
    get_size(Data,Max,_),
    generate_coords(Max,-1,NewCoords1,0),
    generate_coords(Max,Max,NewCoords2,0),
    append(NewCoords1,NewCoords2,NewCoords).

get_new_coords(Data,1,NewCoords):-
    get_size(Data,Max,_),
    generate_coords(-1,Max,NewCoords1,1),
    generate_coords(Max,Max,NewCoords2,1),
    append(NewCoords1,NewCoords2,NewCoords).

% Get the direction of the board (if player 0 or 1 has the turn)
state_direction(Data,0):-
    get_turn(Data,Turn),
    get_orientation(Data,Turn,_).

state_direction(Data,1):-
    get_turn(Data,Turn),
    get_orientation(Data,_,Turn).


% Get all coords in bound
get_all_coords_in_bound(Data,Coords):-
    get_size(Data,Xsize,Ysize),
    generate_to_from(0,Xsize,Xlist),
    generate_to_from(0,Ysize,Ylist),
    generate_pairs(Xlist,Ylist,Coords).

% Get all coords from a list of tiles
get_coords_from_tiles([],[]):- !.
get_coords_from_tiles([Tile|Tiles],[X/Y|Coords]):-
        get_tile_coord(Tile,X,Y),
        get_coords_from_tiles(Tiles, Coords).

% Get all coords that are free, given all Coords in the bounds and the coords already filled in.
get_all_free_coords(Coords, [], Coords).
get_all_free_coords([C|Coords], [C|Taken], Result):-
    get_all_free_coords(Coords,Taken,Result), !.
get_all_free_coords([C|Coords], Taken, [C|Result]):-
    get_all_free_coords(Coords,Taken,Result).

% Generate the new states from a list of options and the current state
% generate_states(A,[X/Y|B],_):- write("Genstates:"), write(B), write(X/Y), write("\n").
generate_states(_,[],[]).
generate_states(Data,[X/Y|Options],[NewState|States]):-
    get_turn(Data,Turn),
    new_tile(Tile,X,Y,Turn),
    add_tile(Data,Tile,NewState),
    generate_states(Data,Options,States).

% Get all possible next states
get_all_states(Data,States):-
    state_direction(Data,Direction),
    get_tiles(Data,Tiles),
    get_new_coords(Data,Direction,NewCoords),
    get_all_coords_in_bound(Data, Coords),
    get_coords_from_tiles(Tiles, Taken_Coords),
    sort(Taken_Coords, Taken_Coords_Sorted),
    get_all_free_coords(Coords, Taken_Coords_Sorted, FreeCoords),
    append(FreeCoords,NewCoords,AllCoords),
    get_neigh_coords_from_list(AllCoords, FreeCoords, Options),
    sort(Options, SortedOptions),
    generate_states(Data,SortedOptions,States).

get_best_state(Data,State):- fail.
