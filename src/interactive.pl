:- module(interactive, [
        start_interactive_game/0
    ]).
:- use_module(library(random)).

get_default_board(Board, Size) :-
    % Return the default board of a certain size, default board has players "red" and "blue"
    % The turn is randomly chosen
    write("Flipping coin to determine if the bot or you will start...\n"),
    random_member(Turn, ["red", "blue"]),
    % Then the board is returned
    new_board(Board, (Size, Size), Turn, ("red", "blue"), 0, []).

start_interactive_game :-
    write("Starting an interactive game of con-tac-tix\n"),
    % Parse the size from stdin
    get_size_from_input(Size),
    write("Size is: "), write(Size), write("\n"),
    % Get the default board of a certain size
    get_default_board(Board, Size),

    % Print the initial board
    print_board_art(Board),
    % Give the player some information about which player they are and which direction they play
    write("You are the red player, the bot is the blue player, you try to connect up and down, the bot left and right\n"),
    % Start the game by doing the initial move
    do_move(Board).

get_size_from_input(Number):-
    write("Please select a size (1-26) of the playing field:"),
    % Read the line from stdin
    read_line_to_string(user_input, String),
    % Parse the line as a number using atom_number
    atom_number(String, Number),
    % Check if the number is between 1 and 26 and ifso cut to stop recursion
    Number > 0, Number =< 26, !.
% As long as we don't get the size parsed correctly we keep trying
get_size_from_input(Number):-
    get_size_from_input(Number).

% Parse the X and Y coordinate from the string
string_to_coord(String, X/Y) :-
    % Chop of the first character of String
    atom_codes(String, [N|Number_codes]),
    % Reconstruct the tailstring from it
    atom_codes(Number_string, Number_codes),
    % Parse the number from the tailstring
    atom_number(Number_string, Number),
    % Calculate the values from the first letter and the number
    char_code("A", A), X is N-A, Y is Number-1.

get_move_from_input(Move_coord, Options) :-
    write("Please select a free coordinate as your next move\n"),
    % Read the line from stdin
    read_line_to_string(user_input, Move),
    % Check if the input parses to a coordinate and it's one of the free coordinates and stop recursion
    string_to_coord(Move, Move_coord),
    member(Move_coord, Options), !.
% As long as we don't get a correct coordinate, we keep trying
get_move_from_input(Move, Options) :-
    get_move_from_input(Move, Options).

% Check if the computer (player 2) has won
check_win_and_exit(2,Board) :-
    % Check if the Board has won
    check_win(Board, New_Board),
    is_win_state(New_Board),
    % And ifso write that the computer has won and exit the program
    write("The bot has won, good luck next time.\n"), halt(0).
% Check if the player (player 1) has won
check_win_and_exit(1,Board) :-
    % Check if the Board has won
    check_win(Board, New_Board),
    is_win_state(New_Board),
    % And ifso write that the player has won and exit the program
    write("Congratulations, you won!!\n"), halt(0).
check_win_and_exit(_,_).

do_move(Board) :-
    % Check if it's the computers turn
    get_turn(Board, Turn),
    get_orientation(Board, _, Turn),
    % Tell the player the computer will compute a move
    write("Computing next move\n"),
    % Compute the next move
    get_best_board(Board, New_Board),
    % Print the new board
    print_board_art(New_Board),
    % Check if the computer won and exit then
    check_win_and_exit(2,New_Board),
    % Otherwise do a new move
    do_move(New_Board), !.
do_move(Board) :-
    % Get the sizes of the board
    get_size(Board, X, Y),
    % Get all coordinates that are within the bounds of the board
    get_all_coords_in_bound(X/Y, Coords),
    % Get all tiles and take their coordinates
    get_tiles(Board, Tiles),
    maplist(get_coord_from_tile, Tiles, Taken_coords),
    % Calculate the free coordinates (and thus the options for the user)
    get_all_free_coords(Coords, Taken_coords, Free_coords),
    % Get the players move from the input
    get_move_from_input(MoveX/MoveY, Free_coords),
    % Take the color of the new tile and the new turn
    get_orientation(Board, Color, Turn),
    % Add the new tile to the board
    new_tile(Tile, MoveX, MoveY, Color),
    add_tile(Board, Tile, New_Board_0),
    % Switch the turn
    set_turn(New_Board_0, Turn, New_Board),
    % Print the art version of the board
    print_board_art(New_Board),
    % Check if the player won and exit then
    check_win_and_exit(1,New_Board),
    % Otherwise do a new move
    do_move(New_Board).
