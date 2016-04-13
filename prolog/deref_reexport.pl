:- module(deref_reexport, [deref_reexport/2]).

:- use_module(library(called_from)).
:- use_module(library(from_utils)).
:- use_module(library(infer_alias)).
:- use_module(library(pretty_decl)).
:- use_module(library(group_pairs_or_sort)).

deref_reexport(Alias, OptionL) :-
    absolute_file_name(Alias, AFile, [file_type(prolog), access(read)]),
    module_property(M, file(AFile)),
    module_property(M, exports(ExL)),
    ( \+ ( member(F/A, ExL),
	   functor(H, F, A),
	   predicate_property(M:H, imported_from(_))
	 )
    ->print_message(information, format("~w does not have reexports", [Alias]))
    ; freeze(H, once((member(F/A, ExL), functor(H, F, A)))),
      collect_called_from(H, RM, _, _, OptionL),
      findall(File/CM,
	      ( called_from:called_from_db(_, RM, CM, _, From),
		RM \= CM,
		( RM = M
		->true
		; predicate_property(M:H, imported_from(RM))
		),
		from_to_file(From, File)
	      ), FileCMU),
      sort(FileCMU, FileCML),
      findall(File/CM-RMPIG,
	      ( member(File/CM, FileCML),
		findall((RM-F/A),
			( called_from:called_from_db(H2, RM, CM, _, From),
			  from_to_file(From, File),
			  RM \= CM,
			  ( RM = M
			  ->true
			  ; predicate_property(M:H, imported_from(RM))
			  ),
			  functor(H2, F, A),
			  \+ file_to_module:declared_use_module(F, A, RM, CM, _, File)
			), RMPIU),
		sort(RMPIU, RMPIL),
		group_pairs_by_key(RMPIL, RMPIG)
	      ), FileRMPIG),
      forall(member(File/CM-RMPIL, FileRMPIG),
	     update_use_module(AFile, M, RMPIL, File, CM))
    ).

update_use_module(AFile, M, RMPIL, File, CM) :-
    module_property(M, exports(ExL)),
    replace_sentence((:- use_module(A)),
		     DeclL,
		     collect_decls(AFile, RMPIL, CM, A, ExL, ExL, DeclL),
		     [file(File)]),
    replace_sentence((:- use_module(A, ImS)),
		     DeclL,
		     collect_decls(AFile, RMPIL, CM, A, ExL, ImS, DeclL),
		     [file(File)]).

collect_decls(AFile, RMPIL, CM, A, ExL, ImS, DeclL) :-
    absolute_file_name(A, AF, [file_type(prolog), access(read)]),
    AF = AFile,
    ( ImS = except(Exc)
    ->subtract(ExL, Exc, ImL)
    ; ImL = ImS
    ),
    ImL \= [],
    findall(PDecl,
	    ( member(RM-RPIL, RMPIL),
	      intersection(RPIL, ImL, PIL),
	      module_property(RM, file(RF)),
	      library_alias(RF, RA),
	      ( \+ ( module_property(RM, exports(ExL)),
		     member(F/A, ExL),
		     \+ member(F/A, PIL),
		     functor(H, F, A),
		     module_property(CM:H, defined)
		   )
	      ->Decl = (:- use_module(RA))
	      ; Decl = (:- use_module(RA, PIL))
	      ),
	      pretty_decl(Decl, PDecl)
	    ), DeclL).
