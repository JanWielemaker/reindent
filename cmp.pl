:- module(swipl_cmp,
	  [ compare/2
	  ]).
:- use_module(library(prolog_source)).

compare(File1, File2) :-
	absolute_file_name(File1, Path1,
			   [ file_type(prolog),
			     access(read)
			   ]),
	absolute_file_name(File2, Path2,
			   [ file_type(prolog),
			     access(read)
			   ]),
	setup_call_cleanup(
	    prepare(State),
	    setup_call_cleanup(
		prolog_open_source(Path1, In1),
		setup_call_cleanup(
		    prolog_open_source(Path2, In2),
		    compare_streams(In1, In2, 0, Errors),
		    prolog_close_source(In2)),
		prolog_close_source(In1)),
	    restore(State)),
	Errors == 0.

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


compare_streams(In1, In2, E0, E) :-
	Options = [syntax_errors(error)],
	prolog_read_source_term(In1, Term1, _Exp1, Options),
	prolog_read_source_term(In2, Term2, _Exp2, Options),
	(   Term1 =@= Term2
	->  E1 = E0
	;   format(user_error, 'MISMATCH: ~n~q \\=@=~n~q~n', [Term1, Term2]),
	    E1 is E0+1
	),
	(   Term1 == end_of_file
	->  E = E1
	;   compare_streams(In1, In2, E1, E)
	).
