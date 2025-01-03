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

:- use_module(library(apply)).
:- use_module(library(main)).

:- use_module('../prolog/expand_tabs').

:- initialization(main,main).

main(Argv) :-
    argv_options(Argv, Files, Options),
    maplist(expand_tabs(Options), Files).

opt_type(tab_distance, tab_distance, natural).

opt_help(help(usage),
         " [--tab_distance=DIST] file ...").
opt_help(help(header),
         md("# Replace tabs by spaces in Prolog source

            Replace tabs by spaces while taking care of spaces in
            Prolog quoted material, including ``0'<tab>``.
            ")).
opt_help(tab_distance,
         "Distance between tab-stops.  Default 8.").

opt_meta(tab_distance, 'DIST').

expand_tabs(Options, File) :-
    get_time(T0),
    format(user_error, 'Expanding ~w ... ', [File]),
    file_expand_tabs(File, File, Options),
    get_time(T1),
    T is T1-T0,
    format(user_error, 'done (~3f sec)~n', [T]).
