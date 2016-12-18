#!/home/jan/bin/swipl

:- use_module(expand).
:- use_module(library(main)).

:- initialization main.

main(Files) :-
    maplist(expand, Files).

expand(File) :-
    get_time(T0),
    format(user_error, 'Expanding ~w ... ', [File]),
    expand(File, File),
    get_time(T1),
    T is T1-T0,
    format(user_error, 'done (~3f sec)~n', [T]).
