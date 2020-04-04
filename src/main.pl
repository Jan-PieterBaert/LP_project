:- use_module(datastructures/ctt).
:- use_module(misc/io_operations).
:- use_module(misc/list_operations).
:- use_module(datastructures/tile).
:- use_module(misc/parse).
:- use_module(solve).

check_data_tiles([],_).
check_data_tiles([Tile|Tiles],L) :-
    get_tile_color(Tile,Color),
    member(Color,L),
    check_data_tiles(Tiles,L).

check_data(Data) :-
    % check that the names of the colors in tiles/turn are correct
    get_orientation(Data,X,Y),
    get_turn(Data,Turn), member(Turn, [X,Y]),
    get_tiles(Data,Tiles),
    check_data_tiles(Tiles,[X,Y]),
    !.

fix_tiles([],[]).
fix_tiles([((X,Y),Color)|Tiles],[NewTile|L]) :-
    new_tile(NewTile,X,Y,Color),
    fix_tiles(Tiles,L).

main :-
    current_prolog_flag(argv, Argv),
    sort(Argv,ArgvSorted),
    main(ArgvSorted).

main(Args) :-
    parse([Size,Turn,Ori,State,Tiles]),
    fix_tiles(Tiles, NewTiles),
    new_data(Data,Size,Turn,Ori,State,NewTiles),
    check_data(Data),
    printlines(Args),
    print_data(Data),
    get_all_states(Data,AllStates),
    printlines(AllStates),
    println("Data:"),
    printlines(Data),
    write("End\n").
