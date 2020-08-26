:- module(board, [
        get_all_boards/2,
        check_win/2,
        get_coord_from_tile/2,
        get_all_free_coords/3,
        get_utility/2,

        get_all_successors/2,
        max_to_move/1,
        min_to_move/1,
        utility/2,

        % Below functions is to be able to use it in the printing of svg's
        get_all_coords_in_bound/2
    ]).

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
    % Take the tiles, turn and size from the data
    get_tiles(Data, Tiles),
    get_turn(Data, Turn),
    get_size(Data, X, Y),
    % Filter based on color, since the turn already switched we exclude the tiles with the same color as the turn
    exclude(is_color(Turn), Tiles, New_Tiles),
    % Take the coordinates of the filtered tiles
    maplist(get_coord_from_tile, New_Tiles, Coords),
    state_direction(Data, Turn, Dir),
    % Because the turn already switched
    Direction is Dir * -1,
    % Take the coordinates that are the borders (left-right or top-bottom)
    get_extra_border_coords(X/Y, Direction, New_border_coords),
    % Make a list of coordinates we need to check
    append(Coords, New_border_coords, Coords_to_check),
    % Apply floodfill to find a path from -1/-1 to X/Y
    Minus_one is -1, floodfill([Minus_one/Minus_one], Coords_to_check, X/Y),
    % Update the value of the data and make a new one
    win_value(Mul), !,
    New_state is Direction * Mul,
    set_state(Data, New_state, New_data).

% when the game is not won, don't change the data
check_win(Data, Data).


% This is the classic floodfill algorithm
%   That means, you take a coordinate and 'fill' all of it's neighbours and do the same recursively for the neighbours
floodfill(Todos, _, Destination) :- member(Destination, Todos).
floodfill([Todo|Todos], Coords, Destination) :-
    % Take all coordinates of all neighbors of the Todo coordinate
    get_neigh_coords(Todo, Coords, Neighs),
    % Remove the neighbors from the coordinates
    remove_elements(Coords, Neighs, New_coords),
    % Debug statement
    % write("Destination:"), write(Destination), write(" | Todos:"), write([Todo|Todos]), write(" | Coords:"), write(Coords), write(" | Neighs:"), write(Neighs), write("\n"),
    % Update the Todos
    append(Todos, Neighs, New_todos),
    % Call floodfill with the new coordinates and new todos
    floodfill(New_todos, New_coords, Destination).

% Helper function to get the X or Y value of the coordinate used in the utility
get_tile_coord_utility(Turn/_, Turn, Tile, Value) :-
    get_tile_coord(Tile, Value, _).
get_tile_coord_utility(_/Turn, Turn, Tile, Value) :-
    get_tile_coord(Tile, _, Value).

% Check if a tile is a certain color
is_color(Color, Tile) :-
    get_tile_color(Tile, Color).

% Calculate the utility value for the data
get_utility(Data, New_data) :-
    % Take the tiles and coordinate from the data
    get_tiles(Data, Tiles),
    get_orientation(Data, X, Y),

    % Get all tiles which are from player 1
    include(is_color(X), Tiles, Tiles_X),
    % Take all the row coordinate parts
    maplist(get_tile_coord_utility(X/Y, Y), Tiles_X, Values_X),
    % Sort, to get a set of it
    sort(Values_X, Sorted_Values_X),
    % Take the length of that set
    length(Sorted_Values_X, Length_X),

    % Get all tiles which are from player 2
    include(is_color(Y), Tiles, Tiles_Y),
    % Take all the column coordinate parts
    maplist(get_tile_coord_utility(X/Y, X), Tiles_Y, Values_Y),
    % Sort, to get a set of it
    sort(Values_Y, Sorted_Values_Y),
    % Take the length of that set
    length(Sorted_Values_Y, Length_Y),

    % The utility is: the number of rows the first player has covered - the number of columns the second player has covered
    Value is Length_Y - Length_X,
    % Update the data value to a new value to give a new data
    set_state(Data, Value, New_data).

% Generate the new states from a list of options and the current state
generate_states(_, _, [], [], _).
generate_states(Data, Turn, [X/Y|Options], [State|States], Direction) :-
    % Add a new tile which is the new option
    new_tile(Tile, X, Y, Turn),
    add_tile(Data, Tile, State),
    % Generate the states for the remaining options
    generate_states(Data, Turn, Options, States, Direction).


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
    generate_states(New_data, Turn, Options, States, Direction).


% Implement the solve interface
get_all_successors(Data, States) :- get_all_boards(Data, States), States \= [].
max_to_move(Data) :- get_turn(Data, Turn), get_orientation(Data, _, Turn).
min_to_move(Data) :- get_turn(Data, Turn), get_orientation(Data, Turn, _).
utility(Data, Value) :- get_utility(Data, New_data), get_state(New_data, Value).

