:- module(parse_source,
          [ parse_source/2,             % +File, -Tree
            sub_node/2,                 % -Sub, +Tree

            check_node/1                % +Tree
          ]).
:- use_module(library(prolog_colour)).
:- use_module(library(prolog_source)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(error)).

:- thread_local
    fragment/3.

parse_source(File, Tree) :-
    absolute_file_name(File, Path,
                       [ file_type(prolog),
                         access(read)
                       ]),
    setup_call_cleanup(
        prepare(State),
        setup_call_cleanup(
            prolog_open_source(Path, In),
            parse_stream(In, Path, Tree0),
            prolog_close_source(In)),
        restore(State)),
    add_layout(Tree0, Tree).

prepare(state(Xref)) :-
    (   current_prolog_flag(xref, Xref)
    ->  true
    ;   Xref = false
    ),
    set_prolog_flag(xref, true).

restore(state(Xref)) :-
    set_prolog_flag(xref, Xref).

parse_stream(In, Path,
             fragment{start:0, end:Len, class:file,
                      string:String, children:Terms}) :-
    stream_property(In, position(Pos0)),
    prolog_colourise_stream(In, Path, on_item),
    set_stream_position(In, Pos0),
    read_string(In, Len, String),
    findall(fragment(Start, Length, Type),
            retract(fragment(Start, Length, Type)),
            Tokens0),
    sort(2, >=, Tokens0, Tokens1),
    sort(1, =<, Tokens1, Tokens),
    fragment_hierarchy(Tokens, String, Terms).

on_item(parentheses, Start, Length) :-
    !,
    End is Start+Length,
    S1 is Start+1,
    E0 is End - 1,
    assert(fragment(Start, S1,  paren_open)),
    assert(fragment(E0,    End, paren_close)),
    assert(fragment(Start, End, parentheses)).
on_item(comment(line), Start, Length) :-       % remove newline from comment
    !,
    End is Start+Length-1,
    assert(fragment(Start, End, comment(line))).
on_item(Type, Start, Length) :-
    End is Start+Length,
    assert(fragment(Start, End, Type)).

%!  fragment_hierarchy(+Fragments, +String, -Hierarchy) is det.
%
%   Translate list of  fragment(Start,  End,   Class)  into  a  dict
%   fragment{start, end, class, string, children}.

fragment_hierarchy([], _, []).
fragment_hierarchy([fragment(S,E,C)|Rest0], String,
                   [fragment{start:S,end:E,class:C,
                             string:FString,children:Sub}|Rest]) :-
    Len is E-S,
    take_sub(String, S, Len, FString),
    sub_fragments(Rest0, E, String, Sub, Rest1),
    fragment_hierarchy(Rest1, String, Rest).

sub_fragments([], _, _, [], []).
sub_fragments([F|R0], End, String, Sub, Rest) :-
    F = fragment(SF,EF,C),
    (   EF =< End
    ->  Len is EF-SF,
        take_sub(String, SF, Len, SubStr),
        Sub = [fragment{start:SF,end:EF,class:C,
                        string:SubStr,children:FSub}|RSub],
        sub_fragments(R0, EF, String, FSub, R1),
        sub_fragments(R1, End, String, RSub, Rest)
    ;   Sub = [],
        Rest = [F|R0]
    ).

take_sub(From, Start, Len, Sub) :-
    sub_string(From, Start, Len, _, Sub), !.
take_sub(From, Start, Len, _Sub) :-
    string_length(From, SLen),
    End is Start + Len,
    domain_error(string_range(SLen), Start-End).

%!  add_layout(+TreeIn, -TreeOut)
%
%   Add the skipped text as layout fragments.

add_layout(TreeIn, TreeOut) :-
    maplist(add_layout, TreeIn.children, Children1),
    add_layout(Children1, TreeIn.start, TreeIn.string, TreeIn.start, AllChildren),
    TreeOut = TreeIn.put(children, AllChildren),
    !.

add_layout([], Offset, _, Offset, []) :- !.
add_layout([], Here, String, Offset, Layout) :-
    Start is Here - Offset,
    sub_string(String, Start, Len, 0, Final),
    (   Len =:= 0
    ->  Layout = []
    ;   End is Here+Len,
        Layout = [ fragment{start:Here, end:End, class:layout,
                            string:Final, children:[]}
                 ]
    ).
add_layout([H|T0], Here, String, Offset, [H|T]) :-
    H.start == Here,
    !,
    add_layout(T0, H.end, String, Offset, T).
add_layout([H|T0], Here, String, Offset,
           [fragment{start:Here, end:End, class:layout,
                     string:Layout, children:[]},H|T]) :-
    End = H.start,
    Len is End - Here,
    Start is Here - Offset,
    sub_string(String, Start, Len, _, Layout),
    add_layout(T0, H.end, String, Offset, T).


                 /*******************************
                 *             QUERY            *
                 *******************************/

sub_node(Node, Node).
sub_node(Sub, Node) :-
    member(Sub0, Node.children),
    sub_node(Sub, Sub0).


                 /*******************************
                 *            TESTING           *
                 *******************************/

check_node(Node) :-
    Node.children == [],
    !.
check_node(Node) :-
    maplist(get_dict(string), Node.children, SubStrings),
    atomics_to_string(SubStrings, Restored),
    NodeString = Node.string,
    assertion(NodeString == Restored),
    maplist(check_node, Node.children).
