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

:- module(reindent,
          [ reindent/2
          ]).
:- use_module(parse_source).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(pprint)).

%!  reindent(+FileIn, +FileOut) is det.
%
%   Reindent  source  for  the  current   SWI-Prolog  "standard"  layout
%   guidelines. This is __not__  a   generic  reindentation,  but merely
%   something  that  updates  from  the  old   to  the  new  layout.  In
%   particular:
%
%     - Indent the clause body 4 spaces rather than a tab.
%     - Put cuts on their own line.

reindent(FileIn, FileOut) :-
    parse_source(FileIn, Tree),
    assertion(check_node(Tree)),
    (   debugging(dump)
    ->  print_term(Tree, [output(user_error)])
    ;   true
    ),
    setup_call_cleanup(
        open(FileOut, write, Out),
        reindent_nodes(Tree.children, Out),
        close(Out)).

reindent_nodes([], _).
reindent_nodes([H|T], Out) :- reindent_node(H, Out), reindent_nodes(T, Out).


reindent_node(Node, Out) :-
    _{class:comment(structured), string:Comment} :< Node,
    !,
    string_codes(Comment, Codes),
    phrase(reindent_comment(Out), Codes).
reindent_node(Node, Out) :-
    _{class:directive} :< Node,
    !,
    leaves(Node, Nodes),
    (   phrase(reindent_directive, Nodes)
    ->  phrase(reindent_clause(Out, _{indent:8}), Nodes)
    ;   format(Out, '~s', [Node.string])
    ).
reindent_node(Node, Out) :-
    has_neck(Node),
    leaves(Node, Nodes),
    phrase(body_indentation(Indent), Nodes),
    !,
    phrase(reindent_clause(Out, _{indent:Indent}), Nodes).
reindent_node(Node, Out) :-
    format(Out, '~s', [Node.string]).

has_neck(Tree) :-
    sub_node(Node, Tree),
    Node.class = neck(_),
    !.

leaves(Tree, Leaves) :-
    findall(node(Class, String),
            leaf_node(Tree, Class, String),
            Leaves0),
    insert_empty_layout(Leaves0, Leaves),
    dump_leaves(Leaves).

insert_empty_layout([], []).
insert_empty_layout([Layout|T0], [Layout|T]) :-
    Layout = node(layout, _),
    !,
    insert_empty_layout(T0, T).
insert_empty_layout([NoLayout1, NoLayout2|T0],
                    [NoLayout1, node(layout, "")|T]) :-
    NoLayout1 \= node(layout, _),
    NoLayout2 \= node(layout, _),
    !,
    insert_empty_layout([NoLayout2|T0], T).
insert_empty_layout([NoLayout1|T0], [NoLayout1|T]) :-
    insert_empty_layout(T0, T).


leaf_node(Tree, Class, String) :-
    sub_node(Node, Tree),
    _{children:[], string:String, class:Class} :< Node.

dump_leaves(Nodes) :-
    (   debugging('dump-leaves')
    ->  print_term(Nodes, [output(user_error)]),
        nl(user_error)
    ;   true
    ).

%!  reindent_comment(+Out)//

reindent_comment(Out) -->
    reindent_line_prefix(Out),
    !,
    copy_to_eol(Out),
    reindent_comment(Out).
reindent_comment(Out) -->
    remainder(List),
    { format(Out, '~s', [List])
    }.

copy_to_eol(Out) -->
    "\n",
    !,
    { format(Out, '~n', []) }.
copy_to_eol(_Out) -->
    eos,
    !.
copy_to_eol(Out) -->
    [C],
    !,
    { put_code(Out, C) },
    copy_to_eol(Out).

reindent_line_prefix(Out) -->
    (   "%%      "
    ;   "%!      "
    ),
    !,
    { format(Out, '%!  ', []) }.
reindent_line_prefix(Out), "\n" -->
    "%", blanks_to_nl,
    !,
    { format(Out, '%', []) }.
reindent_line_prefix(Out) -->
    "%       ",
    !,
    { format(Out, '%   ', []) }.

%!  body_indentation(-Indent)// is semidet.
%
%   Unify Indent with the current leading indentation of the body.

body_indentation(Indent) -->
    string(_),
    neck(_),
    !,
    string(_),
    layout(Layout),
    { sub_string(Layout, _, _, A, "\n"),
      sub_string(Layout, _, A, 0, After),
      split_string(After, "", "\s", [""]),
      !,
      string_length(After, Indent)
    },
    remainder(_).

%!  reindent_clause(+Out, +State)//
%
%   Reindent the remainder of the clause.

reindent_clause(Out, State) -->
    copy_to_neck(Out),
    reindent_body(Out, State.put(paren,[])).

copy_to_neck(_), [node(neck(Type), String)] -->
    [node(neck(Type), String)],
    !.
copy_to_neck(Out) -->
    [node(_, String)],
    !,
    { format(Out, '~s', [String]) },
    copy_to_neck(Out).
copy_to_neck(_) -->
    [].

reindent_body(Out, State) -->
    [ node(paren_open, "(") ],
    !,
    { line_position(Out, Indent),
      format(Out, '(', []),
      push_paren(State, Indent, NewState)
    },
    reindent_body(Out, NewState).
reindent_body(Out, State) -->
    reindent_if_then_else_control(Out, State),
    !,
    reindent_body(Out, State).
reindent_body(Out, State) -->
    [ node(paren_close, ")") ],
    !,
    { format(Out, ')', []),
      pop_paren(State, _OpenIndent, NewState)
    },
    reindent_body(Out, NewState).
reindent_body(Out, State) -->           % do not change layout after ->
    if_then_else(Token),
    layout(S),
    cut,
    !,
    { format(Out, '~s~s!', [Token, S]) },
    reindent_body(Out, State).
reindent_body(Out, State) -->
    layout(Before),
    cut, layout(""), and,
    layout(After),
    { sub_string(After, _, _, _, "\n"),
      split_string(Before, "", "\s\t", [""]),
      split_string(After, "", "\n\s\t", [""]),
      !,
      reindent_layout(Out, After, State),
      format(Out, '!,', []),
      reindent_layout(Out, After, State)
    },
    reindent_body(Out, State).
reindent_body(Out, State) -->
    layout(Before),
    cut, layout(""), and,
    layout(Between),
    [node(comment(line), Comment)],
    layout(After),
    { sub_string(After, _, _, _, "\n"),
      split_string(Before, "", "\s\t", [""]),
      split_string(Between, "", "\s\t", [""]),
      split_string(After, "", "\n\s\t", [""]),
      !,
      format(Out, "  ~s~s", [Between, Comment]),
      reindent_layout(Out, After, State),
      format(Out, '!,', []),
      reindent_layout(Out, After, State)
    },
    reindent_body(Out, State).
reindent_body(Out, _State) -->          % :- !
    neck(Neck),
    layout(Layout),
    cut,
    eos,
    { split_string(Layout, "", "\s\t", [""]),
      !,
      format(Out, '~s !', [Neck,Layout])
    }.
reindent_body(Out, _State) -->          % ! at end of clause
    layout(Layout),
    cut,
    eos,
    { split_string(Layout, "", "\s\t", [""]),
      !,
      format(Out, '\n    !', [])
    }.
reindent_body(Out, State) -->           % in-body full-line comments
    layout("\n"),
    [ node(comment(line), Comment) ], !,
    { format(Out, "\n", []),
      string_codes(Comment, Codes),
      phrase(reindent_comment(Out), Codes)
    },
    reindent_body(Out, State).
reindent_body(Out, State) -->           % Layout
    [node(layout, String)],
    !,
    { reindent_layout(Out, String, State) },
    reindent_body(Out, State).
reindent_body(Out, State) -->           % Any other node
    [node(_, String)],
    !,
    { format(Out, '~s', [String])
    },
    reindent_body(Out, State).
reindent_body(_, _) -->                 % end of input
    [].

reindent_if_then_else_control(Out, State), [node(control,Control)] -->
    layout(Layout), [node(control,Control)],
    { if_then_else_token(Control),
      !,
      pop_paren(State, _OpenIndent, TmpState),
      reindent_layout(Out, Layout, TmpState)
    }.
reindent_if_then_else_control(Out, State), [node(paren_close,")")] -->
    layout(Layout), [node(paren_close,")")],
    !,
    { pop_paren(State, _OpenIndent, TmpState),
      reindent_layout(Out, Layout, TmpState)
    }.

if_then_else_token("->").
if_then_else_token("*->").
if_then_else_token(";").

if_then_else(S) -->
    [ node(control, S) ],
    { if_then_else_token(S) }.

neck(Neck) -->
    [ node(neck(_), Neck) ].

cut -->
    [ node(goal(built_in,!),"!") ].

and -->
    [ node(control, ",") ].

layout(Layout) -->
    [node(layout, Layout)].

%!  reindent_layout(+Out, +Layout, +State)

reindent_layout(Out, String, State) :-
    nl_leading_indent(Leading, State),
    sub_string(String, B, _, A, Leading),
    !,
    sub_string(String, 0, B, _, Lead),
    sub_string(String, _, A, 0, Rest),
    state_paren_count(State, Nest),
    Indent is 4*(Nest+1),
    format(Out, '~s\n~t~*|~s', [Lead, Indent, Rest]).
reindent_layout(Out, String, _) :-
    format(Out, '~s', [String]).

nl_leading_indent(String, State) :-
    state_paren_count(State, Nest),
    InIndent is State.indent + 4*Nest,
    length(Chars, InIndent),
    maplist(=(0'\s), Chars),
    string_codes(String, [0'\n|Chars]).

%!  reindent_directive// is semidet.
%
%   True if this is a directive we need to re-indent.

reindent_directive -->
    [ node(neck(directive), _),
      node(layout, _),
      node(goal(_,_), _),
      node(layout, After)
    ],
    { \+ sub_string(After, 0, _, _, "(") },
    remainder(_).


                 /*******************************
                 *        STATE HANDLING        *
                 *******************************/

push_paren(State, Indent, NewState) :-
    NewState = State.put(paren, [Indent|State.paren]).
pop_paren(State, Indent, NewState) :-
    [Indent|Rest] = State.paren,
    NewState = State.put(paren, Rest).

state_paren_count(State, Nest) :-
    length(State.paren, Nest).
