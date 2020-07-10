:- module(solve, [
        get_all_boards/2,
        get_best_board/2,
        check_win/2,

        % Below functions is to be able to use it in the printing of svg's
        get_all_coords_in_bound/2
    ]).

:- use_module(misc/list_operations).
:- use_module(misc/io_operations).

% For memoization of certain functions
% TODO: add timings about this in the report!
% :- table floodfill/2.


% Get the extra coords for the fields outside the border, where the color starts
%   This means the top and bottom for the first player
%   Or left and right for the second player
get_extra_border_coords(Max_X/Max_Y, -1, New_coords) :-
    findall(X/Y, (between(0,Max_X,X), Y is -1; between(0,Max_X,X), Y is Max_Y), New_coords).
get_extra_border_coords(Max_X/Max_Y, 1, New_coords) :-
    findall(X/Y, (between(0,Max_Y,Y), X is -1; between(0,Max_Y,Y), X is Max_X), New_coords).


% Get the direction of the board this is -1 or 1 (the first or second player respectively)
state_direction(Data, Turn, -1) :-
    get_orientation(Data, Turn, _).
state_direction(Data, Turn, 1) :-
    get_orientation(Data, _, Turn).


% This will return all the coordinates that are within the borders of the Data
%   This means generating the lists 0..Xmax and 0..Ymax and then taking all possible pairs of these lists
get_all_coords_in_bound(Xsize/Ysize, Coords) :-
    Xmax is Xsize - 1, Ymax is Ysize - 1,
    findall(X/Y, (between(0,Xmax,X), between(0,Ymax,Y)), Coords).


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
filter_tiles_to_coords(Tiles, Turn, Coords) :-
    findall(X/Y, (member(Tile, Tiles), new_tile(Tile, X, Y, Turn)), Coords).


% Check if the board is won by the player currently in turn
check_win(Data, New_data) :-
    % Check floadfill
    get_tiles(Data, Tiles),
    get_turn(Data, Turn),
    get_size(Data, X, Y),
    exclude(is_color(Turn), Tiles, New_Tiles),
    maplist(get_coord_from_tile, New_Tiles, [C|Coords]),
    state_direction(Data, Turn, Dir),
    % Because the turn already switched
    Direction is Dir * -1,
    get_extra_border_coords(X/Y, Direction, New_border_coords),
    append(Coords, New_border_coords, Coords_to_check),
    floodfill([C], Coords_to_check),
    win_value(Mul), !,
    New_state is Direction * Mul,
    set_state(Data, New_state, New_data).

% when the game is not won, don't change the data
check_win(Data, Data).


% This is the classic floodfill algorithm
%   That means, you take a coordinate and 'fill' all of it's neighbours and do the same recursively for the neighbours
floodfill(_, []).
floodfill([Todo|Todos], Coords) :-
    get_neigh_coords(Todo, Coords, Neighs),
    remove_elements(Coords, Neighs, New_coords),
    append(Todos, Neighs, New_todos),
    floodfill(New_todos, New_coords).

get_tile_coord_utility(Turn/_, Turn, Tile, Value) :-
    get_tile_coord(Tile, Value, _).
get_tile_coord_utility(_/Turn, Turn, Tile, Value) :-
    get_tile_coord(Tile, _, Value).

is_color(Color, Tile) :-
    get_tile_color(Tile, Color).

get_utility(Data, New_data) :-
    get_turn(Data, Turn),
    get_orientation(Data, X, Y),
    get_tiles(Data, Tiles),
    exclude(is_color(Turn), Tiles, Filtered_Tiles),
    maplist(get_tile_coord_utility(X/Y, Turn), Filtered_Tiles, Values),
    sort(Values, Sorted_Values),
    length(Sorted_Values, Length),
    set_state(Data, Length, New_data).

% Generate the new states from a list of options and the current state
generate_states(_, _, [], _, [], _).
generate_states(Data, Turn, [X/Y|Options], Coords, [New_state|States], Direction) :-
    % Add a new tile which is the new option
    new_tile(Tile, X, Y, Turn),
    add_tile(Data, Tile, State),
    % Check the new board if it's won by the current player
    get_utility(State, New_state),
    % Generate the states for the remaining options
    generate_states(Data, Turn, Options, Coords, States, Direction).


% Get all possible next states
get_all_boards(Data, States) :-
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


% stop_depth(2).
%
% min(State, Val, Depth, Win_state) :-
%     get_all_boards(State, States),
%     minimum(States, Val, Depth, Win_state).
% minimum([New_state|New_states], Val, Depth, Best_state) :-
%     stop_depth(Depth),!,
%     write_debug(["(-)Depth is: ", Depth, "\n"]),
%     write_debug(["At max depth, returning min state\n"]),
%     get_minimal_state_from_list(New_states, New_state, Best_state, 0),
%     get_state(Best_state, Val),
%     write_debug(["We found minimal state: ", Best_state, " with value: ", Val, "\n\n"]).
% minimum([New_state|New_states], Val, Depth, Best_state) :-
%     write_debug(["(-) Depth is: ", Depth, "\n"]),
%     New_depth is Depth + 1,
%     max(New_state, Val1, New_depth, _),
%     get_minimal_state_from_map(New_states, Val1, Best_state, Val, New_depth, New_state),
%     write_debug(["We found minimal state: ", Best_state, "\n"]).
%
% get_minimal_state_from_list(_, Cur_state, Cur_state, Val) :- win_options([-,Val]), !.
% get_minimal_state_from_list([], Cur_state, Cur_state, _).
% get_minimal_state_from_list([State|States], _, Best_state, Cur_val) :-
%     get_state(State, Val),
%     Val < Cur_val, !,
%     get_minimal_state_from_list(States, State, Best_state, Val).
% get_minimal_state_from_list([_|States], State, Best_state, Cur_val) :-
%     get_minimal_state_from_list(States, State, Best_state, Cur_val).
%
% get_minimal_state_from_map([], Val, State, Val, _, State).
% get_minimal_state_from_map(_, Val, State, Val, _, State) :- win_options([_,Val]), !.
% get_minimal_state_from_map([State|States], Cur_val, Best_state, Val, Depth, _) :-
%     max(State, New_val, Depth, _),
%     New_val < Cur_val, !,
%     get_minimal_state_from_map(States, New_val, Best_state, Val, Depth, State).
% get_minimal_state_from_map([_|States], Cur_val, Best_state, Val, Depth, Cur_best) :-
%     get_minimal_state_from_map(States, Cur_val, Best_state, Val, Depth, Cur_best).
%
%
% max(State, Val, Depth, Win_state) :-
%     get_all_boards(State, States),
%     maximum(States, Val, Depth, Win_state).
% maximum([New_state|New_states], Val, Depth, Best_state) :-
%     stop_depth(Depth),!,
%     write_debug(["(+) Depth is: ", Depth, "\n"]),
%     write_debug(["At max depth, returning max state\n"]),
%     get_maximal_state_from_list(New_states, New_state, Best_state, 0),
%     get_state(Best_state, Val),
%     write_debug(["We found maximal state: ", Best_state, " with value: ", Val, "\n\n"]).
% maximum([New_state|New_states], Val, Depth, Best_state) :-
%     write_debug(["(+) Depth is: ", Depth, "\n"]),
%     New_depth is Depth + 1,
%     min(New_state, Val1, New_depth, _),
%     get_maximal_state_from_map(New_states, Val1, Best_state, Val, New_depth, New_state),
%     write_debug(["We found minimal state: ", Best_state, "\n"]).
%
% get_maximal_state_from_list([], Cur_state, Cur_state, _) :- !.
% get_maximal_state_from_list(_, Cur_state, Cur_state, Val) :- win_options([Val,_]), !.
% get_maximal_state_from_list([State|States], _, Best_state, Cur_val) :-
%     Val > Cur_val, !,
%     get_maximal_state_from_list(States, State, Best_state, Val).
% get_maximal_state_from_list([_|States], State, Best_state, Val) :-
%     get_maximal_state_from_list(States, State, Best_state, Val).
%
% get_maximal_state_from_map([], Val, State, Val, _, State).
% get_maximal_state_from_map(_, Val, State, Val, _, State) :- win_options([Val,_]), !.
% get_maximal_state_from_map([State|States], Cur_val, Best_state, Val, Depth, _) :-
%     min(State, New_val, Depth, _),
%     New_val > Cur_val, !,
%     get_maximal_state_from_map(States, New_val, Best_state, Val, Depth, State).
% get_maximal_state_from_map([_|States], Cur_val, Best_state, Val, Depth, Cur_best) :-
%     get_maximal_state_from_map(States, Cur_val, Best_state, Val, Depth, Cur_best).


% If a win is possible in one step, do it
get_best_board(Data, Best_state) :-
    get_all_boards(Data, States),
    maplist(check_win, States, New_States),
    has_win_state(New_States, Best_state).

% Else temporarily just return a state
get_best_board(Data, Best_state) :-
    get_all_boards(Data, [Best_state|_]).

% Else traverse the game tree
get_best_board(Data, Best_state) :-
    get_turn(Data, Turn),
    state_direction(Data, Turn, -1),
    min(Data, Val, 0, Best_state),
    write_debug(["The best board is: ", Best_state, "\n"]),
    write_debug(["Best game solution has a value of: ", Val, "\n"]).

get_best_board(Data, Best_state) :-
    get_turn(Data, Turn),
    state_direction(Data, Turn, 1),
    max(Data, Val, 0, Best_state),
    write_debug(["The best board is: ", Best_state, "\n"]),
    write_debug(["Best game solution has a value of: ", Val, "\n"]).
