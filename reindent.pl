:- module(reindent,
	  [ reindent/2
	  ]).
:- use_module(parse).
:- use_module(library(debug)).
:- use_module(library(dcg/basics)).


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
	_{class:structured_comment, string:Comment} :< Node, !,
	string_codes(Comment, Codes),
	phrase(reindent_comment(Out), Codes).
reindent_node(Node, Out) :-
	_{class:directive} :< Node, !,
	findall(node(Class, String),
		leaf_node(Node, Class, String),
		Nodes),
	dump_leaves(Nodes),
	(   phrase(reindent_directive, Nodes)
	->  phrase(reindent_clause(Out, _{indent:8}), Nodes)
	;   format(Out, '~s', [Node.string])
	).
reindent_node(Node, Out) :-
	has_neck(Node),
	findall(node(Class, String),
		leaf_node(Node, Class, String),
		Nodes),
	dump_leaves(Nodes),
	phrase(body_indentation(Indent), Nodes), !,
	phrase(reindent_clause(Out, _{indent:Indent}), Nodes).
reindent_node(Node, Out) :-
	format(Out, '~s', [Node.string]).

has_neck(Tree) :-
	sub_node(Node, Tree),
	Node.class = neck(_), !.

leaf_node(Tree, Class, String) :-
	sub_node(Node, Tree),
	_{children:[], string:String, class:Class} :< Node.

dump_leaves(Nodes) :-
	(   debugging('dump-leaves')
	->  print_term(Nodes, [output(user_error)]),
	    nl(user_error)
	;   true
	).

%%	reindent_comment(+Out)//

reindent_comment(Out) -->
	reindent_line_prefix(Out), !,
	copy_to_eol(Out),
	reindent_comment(Out).
reindent_comment(Out) -->
	remainder(List),
	{ format(Out, '~s', [List])
	}.

copy_to_eol(Out) -->
	"\n", !,
	{ format(Out, '~n', []) }.
copy_to_eol(Out) -->
	[C], !,
	{ put_code(Out, C) },
	copy_to_eol(Out).

reindent_line_prefix(Out) -->
	(   "%%      "
	;   "%!      "
	), !,
	{ format(Out, '%!  ', []) }.
reindent_line_prefix(Out), "\n" -->
	"%", blanks_to_nl, !,
	{ format(Out, '%', []) }.
reindent_line_prefix(Out) -->
	"%       ", !,
	{ format(Out, '%   ', []) }.

%%	body_indentation(-Indent)// is semidet.
%
%	Unify Indent with the current leading indentation of the body.

body_indentation(Indent) -->
	string(_),
	neck(_), !,
	string(_),
	layout(Layout),
	{ sub_string(Layout, _, _, A, "\n"),
	  sub_string(Layout, _, A, 0, After),
	  split_string(After, "", "\s", [""]), !,
	  string_length(After, Indent)
	},
	remainder(_).

%%	reindent_clause(+Out, +State)//
%
%	Reindent the remainder of the clause.

reindent_clause(Out, State) -->
	copy_to_neck(Out),
	reindent_body(Out, State.put(paren,0)).

copy_to_neck(_), [node(neck(Type), String)] -->
	[node(neck(Type), String)], !.
copy_to_neck(Out) -->
	[node(_, String)], !,
	{ format(Out, '~s', [String]) },
	copy_to_neck(Out).
copy_to_neck(_) -->
	[].

reindent_body(Out, State) -->
	[node(comment, Line), node(layout, Layout0)],
	{ sub_string(Line, 0, _, _, "%"), !,
	  string_concat(Comment, "\n", Line),
	  string_concat("\n", Layout0, Layout),
	  format(Out, '~s', [Comment]),
	  reindent_layout(Out, Layout, State)
	},
	reindent_body(Out, State).
reindent_body(Out, State) -->
	[ node(paren_open, "(") ], !,
	{ format(Out, '(', []),
	  add_field(paren, State, 1, NewState)
	},
	reindent_body(Out, NewState).
reindent_body(Out, State) -->
	reindent_if_then_else_control(Out, State), !,
	reindent_body(Out, State).
reindent_body(Out, State) -->
	[ node(paren_close, ")") ], !,
	{ format(Out, ')', []),
	  add_field(paren, State, -1, NewState)
	},
	reindent_body(Out, NewState).
reindent_body(Out, State) -->				% do not change layout after ->
	if_then_else(Token),
	layout(S),
	cut, !,
	{ format(Out, '~s~s!', [Token, S]) },
	reindent_body(Out, State).
reindent_body(Out, State) -->
	opt_layout(Before),
	cut, and,
	layout(After),
	{ split_string(Before, "", "\s\t", [""]),
	  split_string(After, "", "\n\s\t", [""]), !,
	  reindent_layout(Out, After, State),
	  format(Out, '!,', []),
	  reindent_layout(Out, After, State)
	},
	reindent_body(Out, State).
reindent_body(Out, _State) -->
	neck(Neck),
	opt_layout(Layout),
	cut,
	eos,
	{ split_string(Layout, "", "\s\t", [""]), !,
	  format(Out, '~s !', [Neck,Layout])
	}.
reindent_body(Out, _State) -->
	cut,
	eos, !,
	{ format(Out, '\n    !', [])
	}.
reindent_body(Out, State) -->
	[node(layout, String)], !,
	{ reindent_layout(Out, String, State) },
	reindent_body(Out, State).
reindent_body(Out, State) -->
	[node(_, String)], !,
	{ format(Out, '~s', [String])
	},
	reindent_body(Out, State).
reindent_body(_, _) -->
	[].

reindent_if_then_else_control(Out, State), [node(control,Control)] -->
	layout(Layout), [node(control,Control)],
	{ if_then_else_token(Control), !,
	  add_field(paren, State, -1, TmpState),
	  reindent_layout(Out, Layout, TmpState)
	}.
reindent_if_then_else_control(Out, State), [node(paren_close,")")] -->
	layout(Layout), [node(paren_close,")")], !,
	{ add_field(paren, State, -1, TmpState),
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

opt_layout(Layout) -->
	(   [node(layout, Layout)]
	->  []
	;   { Layout = "" }
	).

%%	reindent_layout(+Out, +Layout, +State)

reindent_layout(Out, String, State) :-
	nl_leading_indent(Leading, State),
	sub_string(String, B, _, A, Leading), !,
	sub_string(String, 0, B, _, Lead),
	sub_string(String, _, A, 0, Rest),
	Indent is 4*(State.paren+1),
	format(Out, '~s\n~t~*|~s', [Lead, Indent, Rest]).
reindent_layout(Out, String, _) :-
	format(Out, '~s', [String]).

leading_indent(String, State) :-
	InIndent is State.indent + 4*State.paren,
	length(Chars, InIndent),
	maplist(=(0'\s), Chars),
	string_codes(String, Chars).

nl_leading_indent(String, State) :-
	InIndent is State.indent + 4*State.paren,
	length(Chars, InIndent),
	maplist(=(0'\s), Chars),
	string_codes(String, [0'\n|Chars]).

%%	reindent_directive// is semidet.
%
%	True if this is a directive we need to re-indent.

reindent_directive -->
	[ node(neck(directive), _),
	  node(layout, _),
	  node(goal(_,_), _),
	  node(layout, After)
	],
	{ \+ sub_string(After, 0, _, _, "(") },
	remainder(_).


add_field(Field, State, Inc, NewState) :-
	New is State.Field+Inc,
	NewState = State.put(Field, New).
