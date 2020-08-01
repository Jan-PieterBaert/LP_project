:- module(solve, [
        get_all_boards/2,
        get_best_board/2,
        check_win/2,
        is_win_state/1,
        get_coord_from_tile/2,
        get_all_free_coords/3,

        % Below functions is to be able to use it in the printing of svg's
        get_all_coords_in_bound/2
    ]).

:- use_module(misc/list_operations).
:- use_module(misc/io_operations).

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
    get_tiles(Data, Tiles),
    get_orientation(Data, X, Y),

    include(is_color(X), Tiles, Tiles_X),
    maplist(get_tile_coord_utility(X/Y, Y), Tiles_X, Values_X),
    sort(Values_X, Sorted_Values_X),
    length(Sorted_Values_X, Length_X),

    include(is_color(Y), Tiles, Tiles_Y),
    maplist(get_tile_coord_utility(X/Y, X), Tiles_Y, Values_Y),
    sort(Values_Y, Sorted_Values_Y),
    length(Sorted_Values_Y, Length_Y),

    % The utility is: the number of rows the first player has covered - the number of columns the second player has covered
    Value is Length_X - Length_Y,
    set_state(Data, Value, New_data).

% Generate the new states from a list of options and the current state
generate_states(_, _, [], _, [], _).
generate_states(Data, Turn, [X/Y|Options], Coords, [State|States], Direction) :-
    % Add a new tile which is the new option
    new_tile(Tile, X, Y, Turn),
    add_tile(Data, Tile, State),
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

% Add alpha/1 and beta/1 as dynamic clauses to store the values of α and β
:- dynamic alpha/1.
:- dynamic beta/1.

% The following code is based on the code from the course
get_all_successors(Data, States) :- get_all_boards(Data, States), States \= [].
min_to_move(Data) :- get_turn(Data, Turn), get_orientation(Data, _, Turn).
max_to_move(Data) :- get_turn(Data, Turn), get_orientation(Data, Turn, _).
utility(Data, Value) :- get_utility(Data, New_data), get_state(New_data, Value).

% At max depth we take the utility of the state
minimax(Pos, _, Val, 0) :-
    update_alpha_beta(Pos),
    utility(Pos, Val), !.
% If β is below α, cut here and return the Position and its value
minimax(Pos, Pos, Val, _) :-
    alpha(Alpha), beta(Beta), Beta =< Alpha, !, utility(Pos, Val).
% Otherwise take the best of all successors
minimax(Pos, BestNextPos, Val, Depth) :-
    get_all_successors(Pos, NextPosList),
    Depth_minus_one is Depth - 1,
    best(NextPosList, BestNextPos, Val, Depth_minus_one), !.
% If the state has no successors, take the utility
minimax(Pos, _, Val, _) :-
    update_alpha_beta(Pos),
    utility(Pos, Val).

update_alpha_beta(Pos) :-
    min_to_move(Pos),   beta(Beta), utility(Pos, Val),
     retract(beta(Beta)),  assert(beta(Val)), !.
update_alpha_beta(Pos) :-
    max_to_move(Pos), alpha(Alpha), utility(Pos, Val),
    retract(alpha(Alpha)), assert(alpha(Val)), !.
update_alpha_beta(_).

best([Pos], Pos, Val, Depth) :-
    update_alpha_beta(Pos),
    minimax(Pos, _, Val, Depth), !.
best([Pos1 | PosList], BestPos, BestVal, Depth) :-
    minimax(Pos1, _, Val1, Depth),
    best(PosList, Pos2, Val2, Depth),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    min_to_move(Pos0),                         % MIN to move in Pos0
    Val0 > Val1, !                             % MAX prefers the greater value
    ;
    max_to_move(Pos0),                         % MAX to move in Pos0
    Val0 < Val1, !.                            % MIN prefers the lesser value
betterOf(_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0


% If a win is possible in one step, do it
get_best_board(Data, Best_state) :-
    get_all_boards(Data, States),
    maplist(check_win, States, New_States),
    has_win_state(New_States, Best_state).

% Else temporarily just return a state
get_best_board(Data, Best_state) :-
    % Set the values for α and β to -1000 and 1000 (as a decent lower and upper bound to start)
    retractall(alpha(_)), retractall(beta(_)),
    assert(alpha(-1000)), assert(beta(1000)),
    debug_print_alpha_beta,
    % Do a minimax with max depth 4
    minimax(Data, Best_state, _, 4),
    debug_print_alpha_beta.

print_if_equal(Data, Data) :-
    write("THEY ARE FUCKING EQUAL\n").
print_if_equal(_, _).

% Define debugging level
% debug(0).
debug(1).
debug_print_alpha_beta:-
    debug(Debug), Debug > 0,
    alpha(Alpha), beta(Beta),
    write("Alpha is: "), write(Alpha), write(" | and beta is: "), write(Beta), write("\n").
debug_print_alpha_beta:-
    debug(0).
