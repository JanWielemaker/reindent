#!/home/jan/bin/swipl

:- use_module(expand_tabs).
:- use_module(library(main)).

:- initialization main.

main(Files) :-
    maplist(expand_tabs, Files).

expand_tabs(File) :-
    get_time(T0),
    format(user_error, 'Expanding ~w ... ', [File]),
    file_expand_tabs(File, File, []),
    get_time(T1),
    T is T1-T0,
    format(user_error, 'done (~3f sec)~n', [T]).
