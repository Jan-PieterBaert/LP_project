% Module which has all functions to get/set elements of the main con-tac-tic data structure
:- module(ctt, [
                win_value/1,win_options/1,
                new_board/1, new_board/6,
                get_size/3, set_size/4,
                get_turn/2, set_turn/3,
                get_orientation/3, set_orientation/4,
                get_state/2, set_state/3,
                get_tiles/2, set_tiles/3, add_tile/3,
                print_data/1
                ]).

:- use_module(tile).

% The possible states which indicate a win
win_value(100).
win_options([A,B]) :- win_value(B), A is -B.

%%% Implementation
% main datastructure: [Size:X/Y, Turn, Orientation:X/Y, State, Tiles:[List of all tiles]]
% Function to give a new data, this can be either empty (the first line) or filled with given values
new_board(board(0/0, "", 0/0, "", [])).
new_board(board(SizeX/SizeY, Turn, OriX/OriY, State, Tiles), (SizeX, SizeY), Turn, (OriX, OriY), State, Tiles).

%% Definitions of getter and setters on the data
% For the size (Horizontal/Vertical)
get_size(board(X/Y, _, _, _, _), X, Y).
set_size(board(_, A, B, C, D), X, Y, board(X/Y, A, B, C, D)).

% For the turn
get_turn(board(_, Turn, _, _, _), Turn).
set_turn(board(A, _, B, C, D), Turn, board(A, Turn, B, C, D)).

% For the orientation (the Horizontal/Vertical player)
get_orientation(board(_, _, X/Y, _, _), X, Y).
set_orientation(board(A, B, _, C, D), X, Y, board(A, B, X/Y, C, D)).

% For the state of the game (undecided/Won by player1/2)
%% The state is between -9 and 9 for a draw game (to allow heuristics), 10 for a game won by player 1 and -10 for a game won by player 2
get_state(board(_, _, _, State, _), State).
set_state(board(A, B, C, _, D), State, board(A, B, C, State, D)).

% For the tiles
get_tiles(board(_, _, _, _, Tiles), Tiles).
set_tiles(board(A, B, C, D, _), Tiles, board(A, B, C, D, Tiles)).
%% And extra to add a tile to the list
add_tile(board(A, B, C, D, Tiles), New_tile, board(A, B, C, D, New_tiles)) :- sort([New_tile|Tiles], New_tiles).

% Determine the string of the state, this is won by player 1/2 with default being "undecided"
determine_state_string(Ori, _, State, State_string):-
    win_options([A,_]),
    State =< A, string_concat("won by ", Ori, State_string).
determine_state_string(_, Ori, State, State_string):-
    win_options([_,A]),
    State >= A, string_concat("won by ", Ori, State_string).
determine_state_string(_, _, _, "undecided").

% Used for printing the state
print_data(board(SizeX/SizeY, Turn, OriX/OriY, State, Tiles)) :-
    determine_state_string(OriX, OriY, State, State_string),
    length(Tiles, L),
    write("tiles: "), write(L), write("\n"),
    maplist(print_tile, Tiles),
    write("state: "), write(State_string), write("\n"),
    write("orientation: "), write(OriX), write(" * "), write(OriY), write("\n"),
    write("size: "), write(SizeX), write(" * "), write(SizeY), write("\n"),
    write("turn: "), write(Turn), write("\n").

%%% Tests for con-tac-tic datastructure
:- begin_tests(ctt_data).
test(size) :-
    % Check the get/set size
    new_board(Data),
    set_size(Data, 10, 20, New_data),
    get_size(New_data, X, Y),
    assertion(X == 10),
    assertion(Y == 20),
    !.

test(turn) :-
    % Check the get/set turn
    new_board(Data),
    set_turn(Data, "Testturn", New_data),
    get_turn(New_data, Turn),
    assertion(Turn == "Testturn"),
    !.

test(orientation) :-
    % Check the get/set orientation
    new_board(Data),
    set_orientation(Data, 10, 20, New_data),
    get_orientation(New_data, X, Y),
    assertion(X == 10),
    assertion(Y == 20),
    !.

test(state) :-
    % Check the get/set state
    new_board(Data),
    set_state(Data, "Teststate", New_data),
    get_state(New_data, State),
    assertion(State == "Teststate"),
    !.

test(tiles) :-
    % Check the get/set/add tiles functions
    new_board(Data0),
    new_tile(T1, 0, 0, ""),
    new_tile(T2, 1, 1, ""),
    new_tile(T3, 2, 2, ""),
    set_tiles(Data0, [T1, T2], Data1),
    get_tiles(Data1, Tiles1),
    assertion(Tiles1 == [T1, T2]),
    add_tile(Data1, T3, Data2),
    get_tiles(Data2, Tiles2),
    assertion(Tiles2 == [T1, T2, T3]),
    !.

:- end_tests(ctt_data).
