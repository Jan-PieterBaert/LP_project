:- use_module(datastructures/ctt).
:- use_module(misc/io_operations).
:- use_module(misc/list_operations).
:- use_module(datastructures/tile).
:- use_module(misc/parse).
:- use_module(solve).

% Check that the color of each tile is valid
check_data_tiles([], _).
check_data_tiles([Tile|Tiles], L) :-
    get_tile_color(Tile, Color),
    member(Color, L),
    check_data_tiles(Tiles, L).

% Check the dataset to see if it's valid
check_data(Data) :-
    % check that the names of the colors in tiles/turn are correct
    get_orientation(Data, X, Y),
    get_turn(Data, Turn), member(Turn, [X, Y]),
    get_tiles(Data, Tiles),
    check_data_tiles(Tiles, [X, Y]),
    !.
% When the data check fails will return exit 3
check_data(_) :-
    exit_with_code_and_message(3, "Invalid dataset").

fix_tiles([], []).
fix_tiles([((X, Y), Color)|Tiles], [New_tile|L]) :-
    new_tile(New_tile, X, Y, Color),
    fix_tiles(Tiles, L).

% Get the states that should be printed, this is all states in case TEST is in the cli Args, otherwise the best state
get_new_states(Data, Args, States) :-
    member('TEST', Args), !,
    get_all_states(Data, States).
get_new_states(Data, _, [State]) :-
    get_best_state(Data, State).

% Print the states, this is in a SVG manner when SVG is in the cli Args, else just to stdout
print_states(_, Args) :-
    member('SVG', Args), !,
    println("SVG output").
print_states(States, _) :-
    print_states(States).
print_states([State]) :-
    print_data(State).
print_states([State|States]) :-
    print_data(State), println("~"),
    print_states(States).

main :-
    current_prolog_flag(argv, Argv),
    sort(Argv, Argv_sorted),
    main(Argv_sorted).

main(Args) :-
    parse([Size, Turn, Ori, State, Tiles]),
    fix_tiles(Tiles, New_tiles),
    new_data(Data, Size, Turn, Ori, State, New_tiles),
    check_data(Data),
    get_new_states(Data, Args, States),
    print_states(States, Args),
    !.
