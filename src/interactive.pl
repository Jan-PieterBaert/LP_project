:- module(interactive, [
        start_interactive_game/0
    ]).
:- use_module(library(random)).

get_default_board(Board, Size) :-
    random_member(Turn, ["red", "blue"]),
    new_board(Board, (Size, Size), Turn, ("red", "blue"), 0, []).


start_interactive_game :-
    write("Starting an interactive game of con-tac-tix\n"),
    % Parse the size from stdin
    get_size_from_input(Size),
    write("Size is: "), write(Size), write("\n"),
    get_default_board(Board, Size),
    print_board_art(Board),
    write("You are the red player, the computer is the blue player, you try to connect up and down, the computer left and right\n"),
    do_move(Board),
    % When the game stops determine who won and exit 0
    write("\nPlayer/Computer won\n"), halt(0).

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

get_move_from_input(Move_coord, Options) :-
    write("Please select a free coordinate as your next move\n"),
    read_line_to_string(user_input, Move),
    string_to_coord(Move, Move_coord),
    member(Move_coord, Options), !.
get_move_from_input(Move, Options) :-
    get_move_from_input(Move, Options).

string_to_coord(String, X/Y) :-
    atom_codes(String, [N|Number_codes]),
    atom_codes(Number_string, Number_codes),
    atom_number(Number_string, Number),
    char_code("A", A), X is N-A, Y is Number-1.

check_win_and_exit(0,Board) :-
    check_win(Board, New_Board),
    is_win_state(New_Board),
    write("The computer has won, good luck next time.\n"), halt(0).
check_win_and_exit(1,Board) :-
    check_win(Board, New_Board),
    is_win_state(New_Board),
    write("Congratulations, you won!!!\n"), halt(0).
check_win_and_exit(_,_).

do_move(Board) :-
    get_turn(Board, Turn),
    get_orientation(Board, _, Turn),
    write("Computer will compute a move\n"),
    get_best_board(Board, New_Board),
    print_board_art(New_Board),
    check_win_and_exit(0,New_Board),
    do_move(New_Board), !.
do_move(Board) :-
    get_size(Board, X, Y),
    get_all_coords_in_bound(X/Y, Coords),
    get_tiles(Board, Tiles),
    get_orientation(Board, Color, Turn),
    maplist(get_coord_from_tile, Tiles, Taken_coords),
    get_all_free_coords(Coords, Taken_coords, Free_coords),
    get_move_from_input(MoveX/MoveY, Free_coords),
    % Add the new tile to the board
    new_tile(Tile, MoveX, MoveY, Color),
    add_tile(Board, Tile, New_Board_0),
    % Switch the turn
    set_turn(New_Board_0, Turn, New_Board),
    % Print the art version of the board
    print_board_art(New_Board),
    check_win_and_exit(1,New_Board),
    do_move(New_Board).
