:- use_module(ctt).
:- use_module(misc).
:- use_module(tile).

main :-
    current_prolog_flag(argv, Argv),
    sort(Argv,ArgvSorted),
    main(ArgvSorted).

main(Args) :-
    read_string(user_input, _, String),
    split_string(String,"\n","",Lines),
    misc:println(""),
    misc:printlines(Lines).

% wat doet read_string(user_input,"\n","\r",_,Codes) ?
