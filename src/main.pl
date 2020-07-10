:- use_module(library(statistics)).
:- use_module(datastructures/ctt).
:- use_module(datastructures/tile).
:- use_module(misc/list_operations).
:- use_module(misc/io_operations).
:- use_module(misc/parse).
:- use_module(misc/svg).
:- use_module(solve).

% Check that the color of each tile is valid
%   This mean that the color of each tile is in Colors (a list of colors given)
check_data_tiles([], _).
check_data_tiles([Tile|Tiles], Colors) :-
    get_tile_color(Tile, Color),
    member(Color, Colors),
    check_data_tiles(Tiles, Colors).

% Check the dataset to see if it's valid
check_data(Data) :-
    % check that the names of the colors in tiles/turn are correct
    get_orientation(Data, X, Y),
    get_turn(Data, Turn), member(Turn, [X, Y]),
    get_tiles(Data, Tiles),
    check_data_tiles(Tiles, [X, Y]),
    % Check that the sizes are positive integers
    get_size(Data, Xmax, Ymax),
    Xmax > 0, Ymax > 0,
    !.

% When the data check fails will return exit 3
check_data(_) :-
    exit_with_code_and_message(3, "Invalid dataset").

tile_from_data(((X, Y), Color), Tile) :- new_tile(Tile, X, Y, Color).
fix_tiles(Data, Tiles) :- maplist(tile_from_data, Data, Tiles).

% Get the states that should be printed, this is all states in case TEST is in the cli Args, otherwise the best state
get_new_boards(Data, Args, New_States) :-
    member('TEST', Args), !,
    get_all_boards(Data, States),
    maplist(check_win, States, New_States).
get_new_boards(Data, _, [State]) :-
    get_best_board(Data, State).


print_svgs([],_) :- print_svg_footer.
print_svgs([State|States], I) :-
    print_svg(State, I),
    I1 is I + 1,
    print_svgs(States, I1).

% Print the states, this is in a SVG manner when SVG is in the cli Args, else just to stdout
print_boards(States, Args) :-
    member('SVG', Args), !,
    print_svg_header,
    print_svgs(States, 0).
print_boards(States, _) :-
    print_boards(States).

% To normally print states, we print each state in the list of states separated by a line with `~`
print_boards([State]) :-
    print_data(State).
print_boards([State|States]) :-
    print_data(State), writeln("~"),
    print_boards(States).

% Get the arguments from the main and call main/1
main :-
    current_prolog_flag(argv, Argv),
    sort(Argv, Argv_sorted),
    main(Argv_sorted).

% Execute the main program
main(Args) :-
    % Parse the data
    parse([Size, Turn, Ori, State, Tiles_data]),
    % Make files from the data
    fix_tiles(Tiles_data, Tiles),
    % Create a board from the data
    new_board(Board, Size, Turn, Ori, State, Tiles),
    % Check if the board is valid
    check_data(Board),
    % Get the new boards (this is all when testing and one when trying to win)
    get_new_boards(Board, Args, States),
    % Print the board in a normal or svg fashion
    print_boards(States, Args).
    % statistics.
