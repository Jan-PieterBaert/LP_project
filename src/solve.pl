:- module(solve, [
        get_best_board/2,
        is_win_state/1
    ]).

:- use_module(misc/list_operations).
:- use_module(misc/io_operations).

% Check if the list of states contains a winning state and ifso return that state.
has_win_state([Win_state|_], Win_state) :-
    is_win_state(Win_state),!.
has_win_state([_|States], Win_state) :-
    has_win_state(States, Win_state).

% Check if a state is won, else fail
is_win_state(State) :-
    get_state(State, Win),
    win_options(Win_options),
    member(Win, Win_options).

% Add alpha/1 and beta/1 as dynamic clauses to store the values of α and β
:- dynamic alpha/1.
:- dynamic beta/1.

% The following code is based on the code from the course "Logisch programmeren" week 7
% Implement the following interface:
% get_all_successors(Data, States).
% max_to_move(Data).
% min_to_move(Data).
% utility(Data, Value).

% At max depth we take the utility of the state
minimax(Pos, _, Val, 0) :-
    update_alpha_beta(Pos),
    utility(Pos, Val),
    % Debug statement
    % write("Minimax_0:\n"), write(Pos), write("\nValue: "), write(Val), write("\n\n"),
    !.
% If β is below α, cut here and return the Position and its value
minimax(Pos, _, Val, _) :-
    alpha(Alpha), beta(Beta), Beta =< Alpha, utility(Pos, Val),%, write("Alpha is: "), write(Alpha), write(" Beta is: "), write(Beta), write("\n").
    % Debug statement
    % write("Minimax_1:\n"), write(Pos), write("\nValue: "), write(Val), write("\n\n"),
    !.
% Otherwise take the best of all successors
minimax(Pos, BestNextPos, Val, Depth) :-
    get_all_successors(Pos, NextPosList),
    Depth_minus_one is Depth - 1,
    % Debug statement
    % write("Next positions are: "), write(NextPosList), write("\n"),
    best(NextPosList, BestNextPos, Val, Depth_minus_one),
    % Debug statement
    % write("Minimax_2:\n"), write(Pos), write("\nValue: "), write(Val), write("\n\n").
    !.
% If the state has no successors, take the utility
minimax(Pos, _, Val, _) :-
    update_alpha_beta(Pos),
    utility(Pos, Val).
    % Debug statement
    % write("Minimax_3:\n"), write(Pos), write("\nValue: "), write(Val), write("\n\n").

% For debugging, uncomment the following line to turn of α-β-pruning
% update_alpha_beta(_) :- !.
update_alpha_beta(Pos) :-
    min_to_move(Pos),   beta(Beta), utility(Pos, Val),
     retract(beta(Beta)),  assert(beta(Val)), !.
update_alpha_beta(Pos) :-
    max_to_move(Pos), alpha(Alpha), utility(Pos, Val),
    retract(alpha(Alpha)), assert(alpha(Val)), !.
update_alpha_beta(_).

best([Pos], Pos, Val, Depth) :-
    minimax(Pos, _, Val, Depth),
    % Debug statement
    % write("Pos is:"), write(Pos), write("\n"), write(Val), write("\n\n"),
    !.
best([Pos1 | PosList], BestPos, BestVal, Depth) :-
    minimax(Pos1, _, Val1, Depth),
    best(PosList, Pos2, Val2, Depth),
    % Debug statement
    % write("Better of: "), write(Depth), write("\n"), write([Pos1, Val1]), write("\n"), write([Pos2, Val2]), write("\n\n"),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    min_to_move(Pos0),                         % MIN to move in Pos0
    Val0 > Val1, !                             % MAX prefers the greater value
    ;
    max_to_move(Pos0),                         % MAX to move in Pos0
    Val0 < Val1, !.                            % MIN prefers the lesser value
betterOf(_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0


% Check if one of the successors has a winning state
get_best_board(Data, Best_state) :-
    % Generate all new boards
    get_all_boards(Data, States),
    % Check if one has a win and return that one
    maplist(check_win, States, New_States),
    has_win_state(New_States, Best_state), !.

% Else do the minimax
get_best_board(Data, Return_state) :-
    % Set the values for α and β to -1000 and 1000 (as a decent lower and upper bound to start)
    retractall(alpha(_)), retractall(beta(_)),
    assert(alpha(-1000)), assert(beta(1000)),
    % Debug statement
    debug_print_alpha_beta,
    % Do a minimax with max depth 4
    minimax(Data, Best_state, Val, -1),
    % Update the win value of the game
    check_win(Best_state, Return_state),
    % Debug statement
    debug_print_final_value(Val),
    debug_print_alpha_beta.

% Define debugging level
debug(0) :- !.
debug(1) :- !.
debug_print_final_value(Val):-
    debug(Debug), Debug > 0,
    write("The final value of the game is: "), write(Val), write("\n").
debug_print_final_value(_):-
    debug(0).
debug_print_alpha_beta:-
    debug(Debug), Debug > 0,
    alpha(Alpha), beta(Beta),
    write("Alpha is: "), write(Alpha), write(" | and beta is: "), write(Beta), write("\n").
debug_print_alpha_beta:-
    debug(0).
