#!/usr/bin/env swipl

:- use_module(expand_tabs).
:- use_module(library(yall)).
:- use_module(reindent).
:- use_module(cmp_source).
:- use_module(library(main)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(dcg/basics)).

:- initialization main.

:- meta_predicate
    run(0, +),
    yesno(0, -).

main(Argv) :-
    argv_options(Argv, Files, Options),
    set_options(Options, RestOptions),
    main(Files, RestOptions).

main(_, Options) :-
    option(help(true), Options),
    help.
main([File], Options) :-
    option(output(Output), Options),
    \+ exists_directory(Output),
    !,
    expand_file(File, Output, Options).
main(Files, Options) :-
    maplist({Options}/[File]>>expand_file(File, Options), Files).

set_options([], []).
set_options([debug(Topic)|T0], T) :-
    !,
    (   Topic == true
    ->  debug
    ;   debug(Topic)
    ),
    set_options(T0, T).
set_options([spy(Spec)|T0], T) :-
    !,
    setup_spy(Spec),
    set_options(T0, T).
set_options([lib(Dir)|T0], T) :-
    !,
    asserta(user:file_search_path(library, Dir)),
    set_options(T0, T).
set_options([pce(true)|T0], T) :-
    !,
    use_module(library(pce)),
    set_options(T0, T).
set_options([H|T0], [H|T]) :-
    !,
    set_options(T0, T).


setup_spy(Spec) :-
    atom_codes(Spec, Codes),
    phrase((string(NameCodes), "/", string(ArityCodes)), Codes),
    catch(number_codes(Arity, ArityCodes), _, fail), !,
    atom_codes(Name, NameCodes),
    guitracer,
    spy(Name/Arity).
setup_spy(Spec) :-
    guitracer,
    spy(Spec).

%!  help
%
%   Print help

help :-
    format('Usage: swi-indent.pl [options] file ...~n', []),
    format('Options:~n~n'),
    format('  --help           Print usage~n'),
    format('  --output=file    Place output in file (only one input)~n'),
    format('  --output=dir     Place output in directory~n'),
    format('  --lib=dir        Add libarry directory~n'),
    format('  --test           Show output in less(1)~n'),
    format('  --dryrun         Do not change any files~n'),
    format('  --force          Ignore changes while comparing~n'),
    format('  --debug=topic    Enable debug topic (may be repeated)~n'),
    format('  --spy=predicate  Spy predicate (may be repeated)~n'),
    format('  --pce            Load library(pce) for colour support~n').

%!  expand_file(+File, +Output, +Options)
%
%   Expand a single file to a target file.

expand_file(File, Output, Options) :-
    format(user_error, 'Reindent ~w (tabs) ... ', [File]),
    run(file_expand_tabs(File, Output, Options), tabs),
    run(reindent(Output, Output), reindent),
    run(cmp_prolog_source(File, Output), cmp),
    format(user_error, '~n', []).

%!  expand_file(+File, +Options) is semidet.
%
%   Expand a single file in-place.

expand_file(File, Options) :-
    option(output(Dir), Options),
    !,
    file_base_name(File, Base),
    directory_file_path(Dir, Base, Output),
    expand_file(File, Output, Options).
expand_file(File, Options) :-
    format(user_error, 'Reindent ~w (tabs) ... ', [File]),
    file_name_extension(File, new, NewFile),
    run(file_expand_tabs(File, NewFile, Options), tabs),
    run(reindent(NewFile, NewFile), reindent),
    (   (   run(cmp_prolog_source(File, NewFile), cmp)
        ;   option(force(true), Options),
            print_message(warning, cmp(ignored))
        )
    ->  finish(File, NewFile, Options)
    ;   fail
    ),
    format(user_error, '~n', []).

finish(_File, NewFile, Options) :-
    option(test(true), Options),
    !,
    process_create(path(less), [file(NewFile)], []),
    delete_file(NewFile).
finish(_File, NewFile, Options) :-
    option(dryrun(true), Options),
    !,
    delete_file(NewFile).
finish(File, NewFile, _Options) :-
    copy_file(NewFile, File),
    delete_file(NewFile).

run(Goal, Name) :-
    format(user_error, ' (~w ', [Name]),
    get_time(T0),
    yesno(Goal, True),
    get_time(T1),
    T is T1-T0,
    (   True == true
    ->  format(user_error, '~3f)', [T])
    ;   True == false,
        format(user_error, 'FAILED)', []),
        fail
    ;   True = error(E),
        message_to_string(E, Msg),
        format(user_error, '~w)', [Msg]),
        fail
    ).

yesno(Goal, YesNo) :-
    catch(Goal, E, true),
    !,
    (   var(E)
    ->  YesNo = true
    ;   YesNo = error(E)
    ).
yesno(_Goal, false).


		 /*******************************
		 *             MESSAGES		*
		 *******************************/

prolog:message(cmp(ignored)) -->
    [ 'Ignored failed comparison'-[] ].
