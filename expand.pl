:- module(expand,
          [ expand/2
          ]).
:- use_module(parse).
:- use_module(library(debug)).
:- use_module(library(dcg/basics)).

expand(FileIn, FileOut) :-
    parse_source(FileIn, Tree),
    assertion(check_node(Tree)),
    findall(node(Class, String),
            leaf_node(Tree, Class, String),
            Nodes),
    setup_call_cleanup(
        open(FileOut, write, Out),
        emit_nodes(Nodes, Out),
        close(Out)).

leaf_node(Tree, Class, String) :-
    sub_node(Node, Tree),
    _{children:[], string:String, class:Class} :< Node.

emit_nodes([], _).
emit_nodes([H|T], Out) :- emit_node(H, Out), emit_nodes(T, Out).

emit_node(node(Class, String), Out) :-
    sub_string(String, _, _, _, "\t"),
    string_codes(String, Codes),
    (   verbatim_class(Class)
    ->  debug(expand, 'Tab in quoted: ~s', [String]),
        quote_tabs(Codes, Out)
    ;   expand_codes(Codes, Out)
    ).
emit_node(node(_Class, String), Out) :-
    format(Out, '~s', [String]).

verbatim_class(string).
verbatim_class(atom).
verbatim_class(int).

%!  expand_codes(+Codes, +Out)
%
%   Expand tabs in Codes.  Tabs are assumed at 8 spaces.

expand_codes([], _).
expand_codes([H|T], Out) :- expand_code(H, Out), expand_codes(T, Out).

expand_code(0'\t, Out) :-
    !,
    line_position(Out, Pos0),
    Pos is ((Pos0+8)//8)*8,
    Spaces is Pos-Pos0,
    forall(between(1, Spaces, _),
           put_code(Out, 0'\s)).
expand_code(C, Out) :-
    put_code(Out, C).

%!  quote_tabs(+Codes, +Out)
%
%   Emit tabs as \t, which is the   preferred style for quoted atoms
%   and strings. Note however that we should translate to spaces for
%   ...\c\n\t...

quote_tabs(Codes, Out) :-
    phrase(quote_tabs(Out), Codes).

quote_tabs(Out) -->
    "\\c", blanks_to_nl,
    !,
    { format(Out, '\\c~n', []) },
    expand_tabs(Out),
    quote_tabs(Out).
quote_tabs(Out) -->
    [0'\t],
    !,
    { format(Out, '\\t', []) },
    quote_tabs(Out).
quote_tabs(Out) -->
    [C],
    !,
    { put_code(Out, C) },
    quote_tabs(Out).
quote_tabs(_) -->
    [].

expand_tabs(Out) -->
    [0'\t],
    !,
    { expand_code(0'\t, Out) },
    expand_tabs(Out).
expand_tabs(_) -->
    [].
