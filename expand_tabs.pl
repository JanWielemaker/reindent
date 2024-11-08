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

:- module(expand_tabs,
          [ file_expand_tabs/3          % +FileIn, +FileOut, +Options
          ]).
:- use_module(parse_source).
:- use_module(library(debug)).
:- use_module(library(dcg/basics)).
:- use_module(library(option)).

%!  file_expand_tabs(+FileIn, +FileOut, +Options) is det.
%
%   Expand tabs to spaces in a file, while  taking care of tabs that may
%   be embedded in quoted material.  Options:
%
%     - tab_distance(+Distance)
%       Distance between tab stops.  Default is 8.

file_expand_tabs(FileIn, FileOut, Options) :-
    parse_source(FileIn, Tree),
    assertion(check_node(Tree)),
    findall(node(Class, String),
            leaf_node(Tree, Class, String),
            Nodes),
    setup_call_cleanup(
        open(FileOut, write, Out),
        emit_nodes(Nodes, Out, Options),
        close(Out)).

leaf_node(Tree, Class, String) :-
    sub_node(Node, Tree),
    _{children:[], string:String, class:Class} :< Node.

emit_nodes([], _, _).
emit_nodes([H|T], Out, Options) :-
    emit_node(H, Out, Options),
    emit_nodes(T, Out, Options).

emit_node(node(Class, String), Out, Options) :-
    sub_string(String, _, _, _, "\t"),
    string_codes(String, Codes),
    (   verbatim_class(Class)
    ->  debug(expand, 'Tab in quoted: ~s', [String]),
        quote_tabs(Codes, Out, Options)
    ;   expand_codes(Codes, Out, Options)
    ).
emit_node(node(_Class, String), Out, _) :-
    format(Out, '~s', [String]).

verbatim_class(string).
verbatim_class(atom).
verbatim_class(int).

%!  expand_codes(+Codes, +Out, +Options)
%
%   Expand tabs in Codes.  Tabs are assumed at 8 spaces.

expand_codes([], _, _).
expand_codes([H|T], Out, Options) :-
    expand_code(H, Out, Options),
    expand_codes(T, Out, Options).

expand_code(0'\t, Out, Options) :-
    !,
    line_position(Out, Pos0),
    option(tab_distance(Tab), Options, 8),
    Pos is ((Pos0+Tab)//Tab)*Tab,
    Spaces is Pos-Pos0,
    forall(between(1, Spaces, _),
           put_code(Out, 0'\s)).
expand_code(C, Out, _) :-
    put_code(Out, C).

%!  quote_tabs(+Codes, +Out, +Options)
%
%   Emit tabs as \t, which is the   preferred style for quoted atoms
%   and strings. Note however that we should translate to spaces for
%   ...\c\n\t...

quote_tabs(Codes, Out, Options) :-
    phrase(quote_tabs(Out, Options), Codes).

quote_tabs(Out, Options) -->
    "\\c", blanks_to_nl,
    !,
    { format(Out, '\\c~n', []) },
    expand_tabs(Out, Options),
    quote_tabs(Out, Options).
quote_tabs(Out, Options) -->
    [0'\t],
    !,
    { format(Out, '\\t', []) },
    quote_tabs(Out, Options).
quote_tabs(Out, Options) -->
    [C],
    !,
    { put_code(Out, C) },
    quote_tabs(Out, Options).
quote_tabs(_, _) -->
    [].

expand_tabs(Out, Options) -->
    [0'\t],
    !,
    { expand_code(0'\t, Out, Options) },
    expand_tabs(Out, Options).
expand_tabs(_, _) -->
    [].
