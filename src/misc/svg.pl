% Print the state of the board as an svg
:- module(svg, [print_svg/1]).

tiles_to_coords([],[]).
tiles_to_coords([Tile|Tiles], [X/Y|Coords]) :-
    get_tile_coord(Tile, X, Y),
    tiles_to_coords(Tiles, Coords).

print_svg(State) :-
    get_size(State, X, Y),
    get_orientation(State, Color_X, Color_Y),

    % Print the header containing some stuff
    print_header(X, Y),

    % --- Starting here it's specific for 1 board

    % Print the translate line
    print_translate(),

    print_players(X, Y, Color_X, Color_Y),

    % Print the row/col indices (ABC/123)
    print_row(X),
    print_col(Y),

    % Fill in all the coordinates with empty tiles
    get_all_coords_in_bound(State, Coords),
    maplist(print_empty_tile, Coords),

    % And overwrite this with filled tiles when necessary
    get_tiles(State, Tiles),
    maplist(print_filled_tile, Tiles),

    % The </g> is per board, the </svg> is general
    print_footer.

print_header(X, Y) :-
    VBox_X is 1.75 * (2 + X + Y/2),
    VBox_Y is 1.5  * (2 + Y),
    writef("<svg width=\"500\" height=\"329.6703296703297\" viewBox=\"0 0 %w %w\" xmlns=\"http://www.w3.org/2000/svg\">", [VBox_X, VBox_Y]),
    writeln("
          <defs>
        <style>
            /* Default tile colours, to make them look like a grid*/
            use:not([fill]) {
                fill: #ECECEC;
            }

            text.row_or_col {
                font-size: 0.3px;
                font-weight: bold;
                font-family: sans;
                fill: white;
                stroke-width: 0.025px;
                paint-order: stroke;
            }
        </style>
        <!-- The tile that is reused (result of right multiplication with inverse of matrix transform)-->
        <polygon id=\"tile\" points=\" 0.35,0.35 0.68,-0.35 0.35,-0.68 -0.35,-0.35 -0.68,0.35 -0.35,0.68 \" stroke-width=\"0.01\" stroke=\"black\"></polygon>
       </defs>
    ").

print_translate() :-
    writef("
        <g transform=\"matrix(1.73205080756 0 0.86602540378 1.5 3.46410161512 2.5)\"><g>
        ").

print_row(-1).
print_row(X) :-
    X1 is X - 1,
    print_row(X1),
    char_code("A", A), N is X+A, atom_codes(L, [N]),
    writef("<text class=\"row_or_col\" y=\"-0.65\" x=\"%w\">%w</text>\n", [X,L]).
print_col(0).
print_col(Y) :-
    Y1 is Y - 1,
    print_col(Y1),
    writef("<text class=\"row_or_col\" x=\"-0.95\" y=\"%w\">%w</text>\n", [Y1,Y]).

print_players(X1, Y1, Color_X, Color_Y) :-
    X is X1 - 1,
    Y is Y1 - 1,
    writef("
<text text-anchor=\"start\" fill=\"%w\" font-size=\"0.5\" y=\"-1.1\" x=\"-0.5\">Player 1</text>
            <polygon fill=\"%w\" points=\"-1,-1 0,0 %w,0 %w,-1\"></polygon>
            <polygon fill=\"%w\" points=\"-1,%w 0,%w %w,%w %w,%w\"></polygon>
<text text-anchor=\"start\" fill=\"%w\" font-size=\"0.5\" transform=\"rotate(90)\" y=\"-%w.1\" x=\"-0.5\">Player 2</text>
            <polygon fill=\"%w\" points=\"-1,-1 0,0 0,%w -1,%w\"></polygon>
            <polygon fill=\"%w\" points=\"%w,-1 %w,0 %w,%w %w,%w\"></polygon></g>\n
                        ", [
                        Color_X,
                        Color_X, X, X1,
                        Color_X, Y1, Y, X, Y, X1, Y1,
                        Color_Y, X1,
                        Color_Y, Y, Y1,
                        Color_Y, X1, X, X, Y, X1, Y1
                        ]).

print_empty_tile(X/Y) :-
    writef("<use href=\"#tile\" x=\"%w\" y=\"%w\"/>\n", [X, Y]).

print_filled_tile(Tile) :-
    get_tile_color(Tile, Color),
    get_tile_coord(Tile, X, Y),
    writef("<use href=\"#tile\" x=\"%w\" y=\"%w\" fill=\"%w\"/>\n", [X, Y, Color]).

print_footer:-
    write("</g></svg>").
