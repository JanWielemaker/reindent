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
	(   current_prolog_flag(xref, Old)
	->  true
	;   Old = false
	),
	setup_call_cleanup(
	    set_prolog_flag(xref, true),
	    setup_call_cleanup(
		prolog_open_source(Path1, In1),
		setup_call_cleanup(
		    prolog_open_source(Path2, In2),
		    compare_streams(In1, In2, 0, Errors),
		    prolog_close_source(In2)),
		prolog_close_source(In1)),
	    set_prolog_flag(xref, Old)),
	Errors == 0.

compare_streams(In1, In2, E0, E) :-
	prolog_read_source_term(In1, Term1, _Exp1, []),
	prolog_read_source_term(In2, Term2, _Exp2, []),
	(   Term1 =@= Term2
	->  E1 = E0
	;   format(user_error, 'MISMATCH: ~n~q \\=@=~n~q~n', [Term1, Term2]),
	    E1 is E0+1
	),
	(   Term1 == end_of_file
	->  E = E1
	;   compare_streams(In1, In2, E1, E)
	).
