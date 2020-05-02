:- module(solve, [
        get_all_states/2,
        get_best_state/2,

        % Below functions is to be able to use it in the printing of svg's
        get_all_coords_in_bound/2
    ]).

:- use_module(misc/list_operations).
:- use_module(misc/io_operations).

% For memoization of certain functions
% TODO: add timings about this in the report!
:- use_module(library(tabling)).
:- table floodfill/2.
:- table get_states/2.


% Generate the extra coords for the fields outside the border, where the color starts, this is used by get_extra_border_coords
% Below are the vertical and horizontal implementations
generate_coords_min_1(-1, _, []) :-!.
generate_coords_min_1(X, Y, [X/Y|Coords]) :-
    X1 is X-1,
    generate_coords_min_1(X1, Y, Coords).
generate_coords_max_1(_, -1, []) :-!.
generate_coords_max_1(X, Y, [X/Y|Coords]) :-
    Y1 is Y-1,
    generate_coords_max_1(X, Y1, Coords).


% Get the extra coords for the fields outside the border, where the color starts
%   This means the top and bottom for the first player
%   Or left and right for the second player
get_extra_border_coords(Max_X/Max_Y, -1, New_coords) :-
    generate_coords_min_1(Max_X, -1   , New_coords1),
    generate_coords_min_1(Max_X, Max_Y, New_coords2),
    append(New_coords1, New_coords2, New_coords).
get_extra_border_coords(Max_X/Max_Y, 1, New_coords) :-
    generate_coords_max_1(-1   , Max_Y, New_coords1),
    generate_coords_max_1(Max_X, Max_Y, New_coords2),
    append(New_coords1, New_coords2, New_coords).


% Get the direction of the board this is -1 or 1 (the first or second player respectively)
state_direction(Data, Turn, -1) :-
    get_orientation(Data, Turn, _).
state_direction(Data, Turn, 1) :-
    get_orientation(Data, _, Turn).


% This will return all the coordinates that are within the borders of the Data
%   This means generating the lists 0..Xmax and 0..Ymax and then taking all possible pairs of these lists
get_all_coords_in_bound(Xsize/Ysize, Coords) :-
    generate_to_from(0, Xsize, Xlist),
    generate_to_from(0, Ysize, Ylist),
    generate_pairs(Xlist, Ylist, Coords).


% Get the coordinate from a tile
get_coord_from_tile(Tile, X/Y) :-
    get_tile_coord(Tile, X, Y).


% Get all coords that are free, given all Coords in the bounds and the coords already filled in.
%   For this we take all coords and the taken coords, when a coord is in the taken coords list, it's not taken in the result
get_all_free_coords(Coords, Taken, Result) :-
    remove_elements(Coords, Taken, Result).


% Return the new turn for the new state
%   This is the other state returned from get_orentation
get_newTurn(Data, Turn, New_turn) :-
    get_orientation(Data, New_turn, Turn);
    get_orientation(Data, Turn, New_turn).


% Filter tiles based on color and give the coordinates
filter_tiles_to_coords([], _, []).
filter_tiles_to_coords([Tile|Tiles], Turn, [X/Y|Coords]) :-
    new_tile(Tile, X, Y, Turn),
    filter_tiles_to_coords(Tiles, Turn, Coords),
    !.
filter_tiles_to_coords([_|Tiles], Turn, Coords) :-
    filter_tiles_to_coords(Tiles, Turn, Coords).


% Check if the board is won by the player currently in turn
check_win([C|Coords], Data, New_data, Direction) :-
    % Check floadfill
    floodfill([C], Coords),
    % The new state for the game is +/- 10 to indicate which player won
    win_value(Mul),
    New_state is Direction * Mul,
    set_state(Data, New_state, New_data),
    !.
% Todo add caching for this?

% when the game is not won, don't change the data
check_win(_, Data, Data, _).


% This is the classic floodfill algorithm
%   That means, you take a coordinate and 'fill' all of it's neighbours and do the same recursively for the neighbours
floodfill(_, []).
floodfill([Todo|Todos], Coords) :-
    get_neigh_coords(Todo, Coords, Neighs),
    remove_elements(Coords, Neighs, New_coords),
    append(Todos, Neighs, New_todos),
    floodfill(New_todos, New_coords).
% Todo add caching for this?


% Generate the new states from a list of options and the current state
generate_states(_, _, [], _, [], _).
generate_states(Data, Turn, [X/Y|Options], Coords, [New_state|States], Direction) :-
    % Add a new tile which is the new option
    new_tile(Tile, X, Y, Turn),
    add_tile(Data, Tile, State),
    % Check the new board if it's won by the current player
    check_win([X/Y|Coords], State, New_state, Direction),
    % Generate the states for the remaining options
    generate_states(Data, Turn, Options, Coords, States, Direction).


% Get all possible next states
get_all_states(Data, States) :-
    % Get the player currently at turn and it's direction
    get_turn(Data, Turn),
    state_direction(Data, Turn, Direction),
    % Get the current tiles
    get_tiles(Data, Tiles),
    % Get the X and Y size of the board
    get_size(Data, X, Y),
    % Get the extra coordinates for the border and all coordinates in bound
    get_extra_border_coords(X/Y, Direction, New_border_coords),
    get_all_coords_in_bound(X/Y, Coords),
    % Generate a list of taken coordinates
    maplist(get_coord_from_tile, Tiles, Taken_coords),
    % Determine the list of all coordinates which are free
    get_all_free_coords(Coords, Taken_coords, Free_coords),
    append(Free_coords, New_border_coords, All_coords),
    % Determine the possible options for a new move
    get_neigh_coords_from_list(All_coords, Free_coords, Options),
    % Flip the turn of the Data, this is to ensure that the newly generated Boards have the right state
    get_newTurn(Data, Turn, New_turn),
    set_turn(Data, New_turn, New_data),
    % Make a list of all coords to check in floodfill
    filter_tiles_to_coords(Tiles, Turn, Colored_tiles),
    append(New_border_coords, Colored_tiles, FF_coords),
    % Generate all states from the list of options
    generate_states(New_data, Turn, Options, FF_coords, States, Direction).


% Check if the list of states contains a winning state and ifso return that state.
has_win_state([Win_state|_], Win_state) :-
    is_win_state(Win_state),!.
has_win_state([_|States], Win_state) :-
    has_win_state(States, Win_state).

is_win_state(State) :-
    get_state(State, Win),
    win_options(Win_options),
    member(Win, Win_options).


stop_depth(3).

min(State, Val, Depth, Win_state) :-
    get_all_states(State, States),
    has_win_state(States, Win_state),!,
    get_state(Win_state, V),
    % A direct win is a better job than a far away win
    Val is V/Depth,
    write_debug(["Found min winning state: ", Win_state, " on depth: ", Depth, " with value: ", Val, "\n"]).
min(State, Val, Depth, Best_state) :-
    stop_depth(Depth),!,
    write_debug(["(-)Depth is: ", Depth, "\n"]),
    write_debug(["At max depth, returning min state\n"]),
    get_all_states(State, [New_state|New_states]),
    get_minimal_state_from_list(New_states, New_state, Best_state),
    get_state(Best_state, V),
    Val is V/Depth,
    write_debug(["We found minimal state: ", Best_state, " with value: ", Val, "\n\n"]).
min(State, Val, Depth, Best_state) :-
    write_debug(["(-) Depth is: ", Depth, "\n"]),
    New_depth is Depth + 1,
    get_all_states(State, [New_state|New_states]),
    max(New_state, Val1, New_depth, _),
    get_minimal_state_from_map(New_states, Val1, Best_state, V, New_depth, New_state),
    Val is V/Depth,
    write_debug(["We found minimal state: ", Best_state, "\n"]).

get_minimal_state_from_list([], Cur_state, Cur_state).
get_minimal_state_from_list([State|States], Cur_state, Best_state) :-
    get_state(State, Val), get_state(Cur_state, Cur_val),
    Val < Cur_val,
    get_minimal_state_from_list(States, State, Best_state), !.
get_minimal_state_from_list([_|States], State, Best_state) :-
    get_minimal_state_from_list(States, State, Best_state).

get_minimal_state_from_map([], Val, State, Val, _, State).
get_minimal_state_from_map([State|States], Cur_val, Best_state, Val, Depth, Cur_best) :-
    max(State, New_val, Depth, _),
    New_val < Cur_val, !,
    get_minimal_state_from_map(States, New_val, Best_state, Val, Depth, State).
get_minimal_state_from_map([_|States], Cur_val, Best_state, Val, Depth, Cur_best) :-
    get_minimal_state_from_map(States, Cur_val, Best_state, Val, Depth, Cur_best).


max(State, Val, Depth, Win_state) :-
    get_all_states(State, States),
    has_win_state(States, Win_state),!,
    get_state(Win_state, V),
    % A direct win is a better job than a far away win
    Val is V/Depth,
    write_debug(["Found max winning state: ", Win_state, " on depth: ", Depth, " with value: ", Val, "\n"]).
max(State, Val, Depth, Best_state) :-
    stop_depth(Depth),!,
    write_debug(["(+) Depth is: ", Depth, "\n"]),
    write_debug(["At max depth, returning max state\n"]),
    get_all_states(State, [New_state|New_states]),
    get_maximal_state_from_list(New_states, New_state, Best_state),
    get_state(Best_state, V),
    Val is V/Depth,
    write_debug(["We found maximal state: ", Best_state, " with value: ", Val, "\n\n"]).
max(State, Val, Depth, Best_state) :-
    write_debug(["(+) Depth is: ", Depth, "\n"]),
    New_depth is Depth + 1,
    get_all_states(State, [New_state|New_states]),
    min(New_state, Val1, New_depth, _),
    get_maximal_state_from_map(New_states, Val1, Best_state, V, New_depth, New_state),
    Val is V/Depth,
    write_debug(["We found minimal state: ", Best_state, "\n"]).

get_maximal_state_from_list([], Cur_state, Cur_state).
get_maximal_state_from_list([State|States], Cur_state, Best_state) :-
    get_state(State, Val), get_state(Cur_state, Cur_val),
    Val > Cur_val,
    get_maximal_state_from_list(States, State, Best_state), !.
get_maximal_state_from_list([_|States], State, Best_state) :-
    get_maximal_state_from_list(States, State, Best_state).

get_maximal_state_from_map([], Val, State, Val, _, State).
get_maximal_state_from_map([State|States], Cur_val, Best_state, Val, Depth, Cur_best) :-
    min(State, New_val, Depth, _),
    New_val > Cur_val, !,
    get_maximal_state_from_map(States, New_val, Best_state, Val, Depth, State).
get_maximal_state_from_map([_|States], Cur_val, Best_state, Val, Depth, Cur_best) :-
    get_maximal_state_from_map(States, Cur_val, Best_state, Val, Depth, Cur_best).


% Currently just return the first possible state if no win is possible
get_best_state(Data, Best_state) :-
    get_turn(Data, Turn),
    state_direction(Data, Turn, -1),
    min(Data, Val, 1, Best_state),
    write_debug(["The best board is: ", Best_state, "\n"]),
    write_debug(["Best game solution has a value of: ", Val, "\n"]).

get_best_state(Data, Best_state) :-
    get_turn(Data, Turn),
    state_direction(Data, Turn, 1),
    max(Data, Val, 1, Best_state),
    write_debug(["The best board is: ", Best_state, "\n"]),
    write_debug(["Best game solution has a value of: ", Val, "\n"]).
