/*  Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2024, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- use_module(library(yall)).
:- use_module(library(main)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(dcg/basics)).

:- use_module('../prolog/expand_tabs').
:- use_module('../prolog/reindent').
:- use_module('../prolog/cmp_source').

:- initialization main.

:- meta_predicate
    run(0, +),
    yesno(0, -).

main(Argv) :-
    argv_options(Argv, Files, Options),
    set_options(Options, RestOptions),
    main(Files, RestOptions).

opt_type(output,       output,       file(write)|directory(write)).
opt_type(tab_distance, tab_distance, natural).
opt_type(lib,          lib,          directory(read)).
opt_type(test,         test,         boolean).
opt_type(dryrun,       dryrun,       boolean).
opt_type(force,        force,        boolean).
opt_type(debug,        debug,        term).
opt_type(spy,          spy,          term).
opt_type(pce,          pce,          boolean).

opt_help(output,
         "File or directory for output").
opt_help(tab_distance,
         "Distance between tab-stops.  Default 8.").
opt_help(lib,
         "Add a library directory").
opt_help(test,
         "Show output using less(1) rather than saving it").
opt_help(dryrun,
         "Do not change any files").
opt_help(debug,
         "Add debug topic").
opt_help(spy,
         "Set spy point on predicate").
opt_help(pce,
         "Load library(pce) for colour support").

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
