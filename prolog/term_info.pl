:- module(term_info, [get_term_info/5]).

% BUG: Files are not uniques
module_files(M, Files) :-
	findall(File, module_file(M, File), UFiles),
	sort(UFiles, Files).

module_file(M, File) :-
	module_file_1(M, File0),
	module_file_2(M, File0, File).

module_file_1(M, File) :-
	module_property(M, file(File)).
module_file_1(M, File) :-
	'$load_context_module'(File, M, _),
	\+ module_property(_, file(File)).

module_file_2(_, File, File).
module_file_2(M, File0, File) :-
	source_file_property(File0, includes(File1,_)),
	module_file_2(M, File1, File).

get_term_info(M, Ref, File, Term, Options) :-
	module_files(M, Files),
	member(File, Files),
	get_term_info_file(Ref, File, Term, [module(M)|Options]).

get_term_info_file(Ref, File, Term, Options) :-
	catch(setup_call_cleanup(open(File, read, In),
				 get_term_info_fd(Ref, In, Term, Options),
				 close(In)),
	      E,
	      (print_message(error, E), fail)).

get_term_info_fd(Ref, In, Term, Options) :-
	repeat,
	catch(read_term(In, Term, Options),
	      E, (print_message(error, E), fail)),
	  ( Term = end_of_file ->
	    !,
	    fail
	  ; subsumes_term(Ref, Term),
	    Ref = Term
	  ).
