% Parsing (or at least a good try) for con-tac-tix
:- module(parse, [parse/1]).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(io_operations).

parse(Retval) :-
    phrase_from_stream(gram(Retval), user_input), !.
% No parse, will return exit 4
parse(_) :-
    exit_with_code_and_message(4, "Failed to parse.").

% When all is parsed, stop and parse the end of input
gram(_, 1, 1, 1, 1, 1) --> !, eos.

% Parse size and indicate that it is parsed
gram([Size, Turn, Ori, State, Tiles], 0, B2, B3, B4, B5) -->
    parse_size(Size),
    gram([Size, Turn, Ori, State, Tiles], 1, B2, B3, B4, B5).

% Parse turn and indicate that it is parsed
gram([Size, Turn, Ori, State, Tiles], B1, 0, B3, B4, B5) -->
    parse_turn(Turn),
    gram([Size, Turn, Ori, State, Tiles], B1, 1, B3, B4, B5).

% Parse orientation and indicate that it is parsed
gram([Size, Turn, Ori, State, Tiles], B1, B2, 0, B4, B5) -->
    parse_orientation(Ori),
    gram([Size, Turn, Ori, State, Tiles], B1, B2, 1, B4, B5).

% Parse state and indicate that it is parsed
gram([Size, Turn, Ori, State, Tiles], B1, B2, B3, 0, B5) -->
    parse_state(State),
    gram([Size, Turn, Ori, State, Tiles], B1, B2, B3, 1, B5).

% Parse tiles and indicate that it is parsed
gram([Size, Turn, Ori, State, Tiles], B1, B2, B3, B4, 0) -->
    parse_tiles(Tiles),
    gram([Size, Turn, Ori, State, Tiles], B1, B2, B3, B4, 1).

% Parse the data by indicating that nothing is parsed yet
gram(Data) -->
    gram(Data, 0, 0, 0, 0, 0).

parse_state(State) -->
    "state:", whites,
    words(State), whites, "\n".

parse_turn(Turn) -->
    "turn:", whites,
    word(Turn), whites, "\n".

parse_size((X, Y)) -->
    "size:", whites,
    integer(X), whites,
    "*", whites,
    integer(Y), whites, "\n".

parse_orientation((C1, C2)) -->
    "orientation:", whites,
    word(C1), whites,
    "*", whites,
    word(C2), whites, "\n".

parse_tile_header(Number_Of_Tiles) -->
    "tiles:", whites,
    integer(Number_Of_Tiles), "\n".
parse_tiles(Tiles) -->
    parse_tile_header(Number_Of_Tiles),
    parse_tile_list(Tiles, Number_Of_Tiles).

parse_tile_coord((L, D1)) -->
    "(", nonblank(C1),
    integer(D), ")",
    {D1 is D-1, char_code("A", C2), L is C1-C2, L >= 0, L < 26 }.

parse_one_tile((Coord, Color)) --> whites,
    parse_tile_coord(Coord), whites,
    "->", whites,
    word(Color), whites, "\n".


parse_tile_list([Tile|[]], 1) -->
    whites, parse_one_tile(Tile).
parse_tile_list([Tile|Tiles], N) -->
    whites, parse_one_tile(Tile),
    { N1 is N-1 },
    parse_tile_list(Tiles, N1).

parse_size(Size1, Size2) --> "size", integer(Size1), "*", integer(Size2), "\n".

word(Word) --> nonblanks(W), {atom_codes(Word, W)}.
words(Word) --> string(W), {atom_codes(Word, W)}.
