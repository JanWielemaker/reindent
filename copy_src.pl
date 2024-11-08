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

:- module(copy_src,
          [ copy_src/2                          % +In, +Out
          ]).
:- use_module(library(prolog_source)).

%!  copy_src(+InFile, +OutFile) is det.
%
%   Copy InFile to OutFile by reading it   term  for term as Prolog term
%   and writing the result. This illustrates   that  we can preserve the
%   file while getting access to its Prolog structure. Indentation tools
%   may use this as a starting point.
%
%   @see parse_source.pl that provides an alternative approach by
%   reading a file into a hierachy of text nodes with syntactical
%   classifications.

copy_src(InFile, OutFile) :-
    setup_call_cleanup(
        open(InFile, read, In),
        setup_call_cleanup(
            open(OutFile, write, Out),
            copy_stream(In, Out),
            close(Out)),
        close(In)).

copy_stream(In, Out) :-
    read_src(In, Clause),
    write_src(Out, Clause),
    (   Clause.term == end_of_file
    ->  !
    ;   copy_stream(In, Out)
    ).

%!  read_src(+In, -Clause) is det.
%
%   Read the next clause from the input

read_src(In, Clause) :-
    stream_property(In, position(Start)),
    prolog_read_source_term(In, Term, _Expanded,
                            [ variable_names(Vars),
                              term_position(StartTerm),
                              subterm_positions(Layout),
                              comments(Comment)
                            ]),
    stream_property(In, position(EndTerm)),
    read_range(In, StartTerm, EndTerm, String),
    Clause = clause{term:       Term,
                    vars:       Vars,
                    input:      In,
                    start:      Start,
                    start_term: StartTerm,
                    end_term:   EndTerm,
                    string:     String,
                    layout:     Layout,
                    comments:   Comment}.

write_src(Out, Clause) :-
    write_leading_comments(Out, Clause),
    write_clause(Out, Clause).

write_leading_comments(Out, Clause) :-
    debug(indent(comment), 'Comments: Start = ~p, Comments = ~p',
          [ Clause.start_term, Clause.comments ]),
    leading_comments(Clause.comments, Clause.start_term, Out).

leading_comments([], _, _) :- !.
leading_comments([CStart-Comment|T], Start, Out) :-
    CStart @< Start,
    !,
    sync_line(Out, CStart),
    format(Out, '~s', [Comment]),
    leading_comments(T, Start, Out).
leading_comments(_, _, _).

write_clause(Out, Clause) :-
    Clause.term == end_of_file,
    !,
    sync_line(Out, Clause.start_term).
write_clause(Out, Clause) :-
    sync_line(Out, Clause.start_term),
    output_term(Clause.term, Clause.layout, 0, Clause.put(out, Out)).

output_term(_Prim, From-To, Here, Clause) :-
    !,
    original(From-To, Clause, String),
    copy_skipped(Here, From, Clause),
    format(Clause.out, '~s', String).
output_term(Compound, term_position(_F,_T,_FF,_FT,Args), Here, Clause) :-
    !,
    output_args(1, Compound, Args, Here, Clause).
output_term(_, _, _).

output_args(_, _, [], _, _) :- !.
output_args(I, Term, [H|T], Here, Clause) :-
    arg(I, Term, Arg),
    output_term(Arg, H, Clause),
    I2 is I+1,
    output_args(I2, Term, T, Here, Clause).


%!  read_range(+In, +Start, +End, -String)
%
%   Read the range Start-End into String.

read_range(In, Start, End, String) :-
    stream_property(In, position(OldPos)),
    set_stream_position(In, Start),
    stream_position_data(char_count, Start, StartChar),
    stream_position_data(char_count, End, EndChar),
    Len is EndChar - StartChar,
    read_string(In, Len, String),
    set_stream_position(In, OldPos).

original(Pos, Clause, String) :-
    stream_position_data(char_count, Clause.start_term, StartTerm),
    arg(1, Pos, Start),
    arg(2, Pos, End),
    CStart is Start - StartTerm,
    Len is End-Start,
    sub_string(Clause.string, CStart, Len, _, String).

%!  sync_line(Out, Pos) is det.
%
%   Insert newlines to get Output at the same line as Pos.

sync_line(Out, Pos) :-
    stream_property(Out, position(Here)),
    stream_position_data(line_count, Here, Line0),
    stream_position_data(line_count, Pos, Line),
    Need is Line-Line0,
    forall(between(1, Need, _), nl(Out)).

%!  copy_skipped(+Here, +From, +Clause) is det.

copy_skipped(Here, From, Clause) :-
    original(Here-From, Clause, String),
    format(Clause.out, '~s', String).
