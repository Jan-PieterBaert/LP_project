:- module(io_operations, [
        exit_with_code_and_message/2,
        write_debug/1
    ]).

% Print a message to stderr and exit with a certain exitcode
exit_with_code_and_message(Code, Error_message) :-
    set_output(user_error),
    writeln(Error_message),
    halt(Code).


% When the following line is commented, debug statements will be printed
write_debug(_).

% write_debug([]).
% write_debug([H|T]) :-
%     write(H), write_debug(T).
