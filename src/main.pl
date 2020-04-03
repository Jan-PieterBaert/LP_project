:- use_module(datastructures/ctt).
:- use_module(misc/list_operations).
:- use_module(datastructures/tile).

main :-
    current_prolog_flag(argv, Argv),
    sort(Argv,ArgvSorted),
    main(ArgvSorted).

main(Args) :-
    read_string(user_input, _, String),
    split_string(String,"\n","",Lines),
    println(""),
    printlines(Lines).

% wat doet read_string(user_input,"\n","\r",_,Codes) ?
