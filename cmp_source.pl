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

:- module(cmp_source,
          [ cmp_prolog_source/2
          ]).
:- use_module(library(prolog_source)).

%!  cmp_prolog_source(+File1, +File2) is semidet.
%
%   Compare two source files to  be  identical   in  the  sense that all
%   Prolog terms are variants  of  each   other.  I.e.,  this  validates
%   equivalence  as  Prolog  source,  ignoring    comments,  layout  and
%   syntactical changes that do not affect the term read.

cmp_prolog_source(File1, File2) :-
    source_file_to_terms(File1, Terms1),
    source_file_to_terms(File2, Terms2),
    compare_terms(Terms1, Terms2, 0, E),
    E == 0.

source_file_to_terms(File, Terms) :-
    absolute_file_name(File, Path,
                       [ file_type(prolog),
                         access(read)
                       ]),
    setup_call_cleanup(
        prepare(State),
        setup_call_cleanup(
            prolog_open_source(Path, In),
            read_source_terms(In, Terms),
            prolog_close_source(In)),
        restore(State)).

prepare(state(Xref,SingleTon)) :-
    (   current_prolog_flag(xref, Xref)
    ->  true
    ;   Xref = false
    ),
    (   style_check(?(singleton))
    ->  SingleTon = true
    ;   SingleTon = false
    ),
    set_prolog_flag(xref, true),
    style_check(-singleton).

restore(state(Xref,SingleTon)) :-
    set_prolog_flag(xref, Xref),
    (   SingleTon == true
    ->  style_check(+singleton)
    ;   style_check(-singleton)
    ).

read_source_terms(In, Terms) :-
    at_end_of_stream(In),
    !,
    Terms = [].
read_source_terms(In, [@(File:Line,H)|T]) :-
    prolog_read_source_term(In, H, _Exp1, [syntax_errors(error)]),
    source_location(File, Line),
    read_source_terms(In, T).

compare_terms([], [], E, E).
compare_terms([@(P0,H0)|T0], [@(_P1,H1)|T1], E0, E) :-
    (   H0 =@= H1
    ->  E1 is E0
    ;   print_message(error, cmp(diff(P0, H0, H1))),
        E1 is E0+1
    ),
    compare_terms(T0, T1, E1, E).


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

prolog:message(cmp(diff(File:Line, Term1, Term2))) -->
    [ 'MISMATCH: ~w:~w'-[File, Line], nl,
      '~q \\=@='-[Term1], nl,
      '~q'-[Term2]
    ].
