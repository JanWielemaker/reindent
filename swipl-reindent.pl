:- module(reindent,
	  [ reindent/2
	  ]).
:- use_module(parse).
:- use_module(library(debug)).
:- use_module(library(dcg/basics)).


reindent(FileIn, FileOut) :-
	parse_source(FileIn, Tree),
	assertion(check_node(Tree)),
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
	_{class:clause} :< Node, !,
	findall(node(Class, String),
		leaf_node(Node, Class, String),
		Nodes),
	phrase(reindent_clause(Out), Nodes).
reindent_node(Node, Out) :-
	_{class:grammar_rule} :< Node, !,
	findall(node(Class, String),
		leaf_node(Node, Class, String),
		Nodes),
	phrase(reindent_clause(Out), Nodes).
reindent_node(Node, Out) :-
	_{class:directive} :< Node,
	findall(node(Class, String),
		leaf_node(Node, Class, String),
		Nodes),
	phrase(reindent_directive, Nodes), !,
	phrase(reindent_clause(Out), Nodes).
reindent_node(Node, Out) :-
	format(Out, '~s', [Node.string]).

leaf_node(Tree, Class, String) :-
	sub_node(Node, Tree),
	_{children:[], string:String, class:Class} :< Node.

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

%%	reindent_clause(Out)//
%
%	Reindent the remainder of the clause.

reindent_clause(Out) -->
	copy_to_neck(Out),
	reindent_body(Out).

copy_to_neck(Out) -->
	[node(neck(_), String)], !,
	{ format(Out, '~s', [String]) }.
copy_to_neck(Out) -->
	[node(_, String)], !,
	{ format(Out, '~s', [String]) },
	copy_to_neck(Out).
copy_to_neck(_) -->
	[].

reindent_body(Out) -->
	[node(comment, Line), node(layout, String)],
	{ sub_string(Line, 0, _, _, "%"),
	  sub_string(String, 0, _, A, "        "), !,
	  sub_string(String, _, A, 0, Rest),
	  format(Out, '~s    ~s', [Line, Rest])
	},
	reindent_body(Out).
reindent_body(Out) -->
	[node(layout, String)],
	{ sub_string(String, B, _, A, "\n        "), !,
	  sub_string(String, 0, B, _, Lead),
	  sub_string(String, _, A, 0, Rest),
	  format(Out, '~s\n    ~s', [Lead, Rest])
	},
	reindent_body(Out).
reindent_body(Out) -->
	[node(_, String)], !,
	{ format(Out, '~s', [String])
	},
	reindent_body(Out).
reindent_body(_) -->
	[].

reindent_directive -->
	[ node(neck(directive), _),
	  node(layout, _),
	  node(goal(_,_), _),
	  node(layout, After)
	],
	{ \+ sub_string(After, 0, _, _, "(") },
	remainder(_).
