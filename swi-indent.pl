#!/home/jan/bin/swipl

:- use_module(expand).
:- use_module(reindent).
:- use_module(cmp).
:- use_module(library(main)).

:- initialization main.

:- meta_predicate
	run(0, +),
	yesno(0, -).

main(Files) :-
	maplist(expand, Files).

expand(File) :-
	format(user_error, 'Reindent ~w (tabs) ... ', [File]),
	file_name_extension(File, new, NewFile),
	run(expand(File, NewFile), tabs),
	run(reindent(NewFile, NewFile), reindent),
	(   run(compare(File, NewFile), cmp)
	->  copy_file(NewFile, File),
	    delete_file(NewFile)
	;   fail
	),
	format(user_error, '~n', []).

run(Goal, Name) :-
	format(user_error, ' (~w ', [Name]),
	get_time(T0),
	yesno(Goal, True),
	get_time(T1),
	T is T1-T0,
	(   True == true
	->  format(user_error, '~3f)', [T])
	;   format(user_error, 'FAILED)', []),
	    fail
	).

yesno(Goal, YesNo) :-
	call(Goal), !,
	YesNo = true.
yesno(_Goal, false).
