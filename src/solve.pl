:- module(solve, [
        get_all_states/2,
        get_best_state/2
    ]).

:- use_module(misc/list_operations).

% Generate the extra coords for the fields outside the border, where the color starts
generate_coords(0, _, [], 0) :-!.
generate_coords(X, Y, [X/Y|Coords], 0) :-
    X1 is X-1,
    generate_coords(X1, Y, Coords, 0).

generate_coords(_, 0, [], 1) :-!.
generate_coords(X, Y, [X/Y|Coords], 1) :-
    Y1 is Y-1,
    generate_coords(X, Y1, Coords, 1).

% Get the extra coords for the fields outside the border, where the color starts
get_new_coords(Data, 0, New_coords) :-
    get_size(Data, Max, _),
    generate_coords(Max, -1, New_coords1, 0),
    generate_coords(Max, Max, New_coords2, 0),
    append(New_coords1, New_coords2, New_coords).

get_new_coords(Data, 1, New_coords) :-
    get_size(Data, Max, _),
    generate_coords(-1, Max, New_coords1, 1),
    generate_coords(Max, Max, New_coords2, 1),
    append(New_coords1, New_coords2, New_coords).

% Get the direction of the board (if player 0 or 1 has the turn)
state_direction(Data, 0) :-
    get_turn(Data, Turn),
    get_orientation(Data, Turn, _).

state_direction(Data, 1) :-
    get_turn(Data, Turn),
    get_orientation(Data, _, Turn).


% Get all coords in bound
get_all_coords_in_bound(Data, Coords) :-
    get_size(Data, Xsize, Ysize),
    generate_to_from(0, Xsize, Xlist),
    generate_to_from(0, Ysize, Ylist),
    generate_pairs(Xlist, Ylist, Coords).

% Get all coords from a list of tiles
get_coords_from_tiles([], []) :- !.
get_coords_from_tiles([Tile|Tiles], [X/Y|Coords]) :-
        get_tile_coord(Tile, X, Y),
        get_coords_from_tiles(Tiles, Coords).

% Get all coords that are free, given all Coords in the bounds and the coords already filled in.
get_all_free_coords(Coords, [], Coords).
get_all_free_coords([C|Coords], [C|Taken], Result) :-
    get_all_free_coords(Coords, Taken, Result), !.
get_all_free_coords([C|Coords], Taken, [C|Result]) :-
    get_all_free_coords(Coords, Taken, Result).

% Return the new turn for the new state
get_newTurn(Data, Turn, New_turn) :-
    get_orientation(Data, New_turn, Turn).

get_newTurn(Data, Turn, New_turn) :-
    get_orientation(Data, Turn, New_turn).

% Filter tiles based on color
filter_tiles_to_coords([], _, []).
filter_tiles_to_coords([Tile|Tiles], Turn, [X/Y|Coords]) :-
    new_tile(Tile, X, Y, Turn),
    filter_tiles_to_coords(Tiles, Turn, Coords),
    !.
filter_tiles_to_coords([_|Tiles], Turn, Coords) :-
    filter_tiles_to_coords(Tiles, Turn, Coords).


% Check if the board is won by one of the players
check_win([C|Coords], Data, Turn, New_data) :-
    % Check floadfill
    % write("\n_data:"), write(Data), write("\n"),
    % write("Coords"), write(Coords), write("\n"),
    % write("Turn:"), write(Turn), write("\n"),
    floodfill([C], Coords),
    % write("WIN!\n"),
    string_concat("won by ", Turn, New_state),
    set_state(Data, New_state, New_data),
    % write("Data:"), write(New_data), write("\n"),
    !.
check_win(_, Data, _, Data).

floodfill(_, []).
floodfill([Todo|Todos], Coords) :-
    get_neigh_coords(Todo, Coords, Neighs),
    remove_elements(Coords, Neighs, New_coords),
    append(Todos, Neighs, New_todos),
    % write("Coords: "), write(Coords), write(" | "), write(New_coords), write("\n"),
    % write("Todos: "), write(Todo), write(" | "), write(New_todos), write("\n"),
    % write("Neigh: "), write(Neighs), write("\n"),
    % write("---\n"),
    floodfill(New_todos, New_coords),
    !.

% Generate the new states from a list of options and the current state
% generate_states(A, [X/Y|B], _) :- write("Genstates:"), write(B), write(X/Y), write("\n").
generate_states(_, _, [], _, []).
generate_states(Data, Turn, [X/Y|Options], Coords, [New_state|States]) :-
    new_tile(Tile, X, Y, Turn),
    add_tile(Data, Tile, State),
    check_win([X/Y|Coords], State, Turn, New_state),
    generate_states(Data, Turn, Options, Coords, States).

% Get all possible next states
get_all_states(Data, States) :-
    state_direction(Data, Direction),
    get_tiles(Data, Tiles),
    get_new_coords(Data, Direction, New_coords),
    get_all_coords_in_bound(Data, Coords),
    get_coords_from_tiles(Tiles, Taken_Coords),
    sort(Taken_Coords, Taken_Coords_Sorted),
    get_all_free_coords(Coords, Taken_Coords_Sorted, Free_coords),
    append(Free_coords, New_coords, All_coords),
    get_neigh_coords_from_list(All_coords, Free_coords, Options),
    sort(Options, Sorted_options),
    get_turn(Data, Turn),
    get_newTurn(Data, Turn, New_turn),
    set_turn(Data, New_turn, New_data),
    filter_tiles_to_coords(Tiles, Turn, Check_winCoords1),
    append(New_coords, Check_winCoords1, Check_winCoords),
    generate_states(New_data, Turn, Sorted_options, Check_winCoords, States).

has_win_state([Win_state|_], Win_state) :-
    get_state(Win_state, Win),
    string_concat("won by", _, Win),
    !.
has_win_state([_|States], Win_state) :-
    has_win_state(States, Win_state).

get_best_state(Data, Win_state) :-
    get_all_states(Data, States),
    has_win_state(States, Win_state),
    !.

get_best_state(Data, State) :-
    % Currently just return the first possible state if no win is possible
    get_all_states(Data, [State|_]).
