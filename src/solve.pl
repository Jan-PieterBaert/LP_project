:- module(solve, [
        get_all_states/2,
        get_best_state/2
    ]).

:- use_module(misc/list_operations).
:- use_module(misc/io_operations).

% Generate the extra coords for the fields outside the border, where the color starts
generate_coords_0(-1, _, []) :-!.
generate_coords_0(X, Y, [X/Y|Coords]) :-
    X1 is X-1,
    generate_coords_0(X1, Y, Coords).

generate_coords_1(_, -1, []) :-!.
generate_coords_1(X, Y, [X/Y|Coords]) :-
    Y1 is Y-1,
    generate_coords_1(X, Y1, Coords).

% Get the extra coords for the fields outside the border, where the color starts
get_new_coords(Data, 0, New_coords) :-
    get_size(Data, Max_X, Max_Y),
    generate_coords_0(Max_X, -1   , New_coords1),
    generate_coords_0(Max_X, Max_Y, New_coords2),
    append(New_coords1, New_coords2, New_coords).

get_new_coords(Data, 1, New_coords) :-
    get_size(Data, Max_X, Max_Y),
    generate_coords_1(-1   , Max_Y, New_coords1),
    generate_coords_1(Max_X, Max_Y, New_coords2),
    append(New_coords1, New_coords2, New_coords).

% Get the direction of the board (if player 0 or 1 has the turn)
state_direction(Data, Turn, 0) :-
    get_orientation(Data, Turn, _).

state_direction(Data, Turn, 1) :-
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


% Check if the board is won by the player currently in turn
check_win([C|Coords], Data, Turn, New_data) :-
    % Check floadfill
    write_debug([
    "\n_data:", Data, "\n",
    "Coords", Coords, "\n",
    "Turn:", Turn, "\n",
    "Coords:", [C|Coords], "\n"
    ]),
    floodfill([C], Coords),
    string_concat("won by ", Turn, New_state),
    set_state(Data, New_state, New_data),
    write_debug(["Game won by ", Turn, "\n"]),
    !.
% else
check_win(_, Data, Turn, Data) :-
    write_debug(["Game not won by ", Turn, "\n"]).

floodfill(_, []) :- !.
floodfill([Todo|Todos], Coords) :-
    get_neigh_coords(Todo, Coords, Neighs),
    remove_elements(Coords, Neighs, New_coords),
    append(Todos, Neighs, New_todos),
    write_debug([
    "Coords: ", Coords, " | ", New_coords, "\n",
    "Todos: ", Todo, " | ", New_todos, "\n",
    "Neigh: ", Neighs, "\n",
    "---\n"
    ]),
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
    get_turn(Data, Turn),
    state_direction(Data, Turn, Direction),
    get_tiles(Data, Tiles),
    get_new_coords(Data, Direction, New_coords),
    get_all_coords_in_bound(Data, Coords),
    get_coords_from_tiles(Tiles, Taken_Coords),
    sort(Taken_Coords, Taken_Coords_Sorted),
    get_all_free_coords(Coords, Taken_Coords_Sorted, Free_coords),
    append(Free_coords, New_coords, All_coords),
    get_neigh_coords_from_list(All_coords, Free_coords, Options),
    sort(Options, Sorted_options),
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
