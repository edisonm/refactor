/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(file_to_module, [file_to_module/1, file_to_module/2]).

:- use_module(library(prolog_metainference)).
:- use_module(xlibrary(clambda)).
:- use_module(xlibrary(infer_alias)).
:- use_module(xlibrary(list_sequence)).
:- use_module(xlibrary(remove_dups)).
:- use_module(xlibrary(sequence_list)).
:- use_module(xtools(extra_codewalk)).
:- use_module(xtools(extra_location)).
:- use_module(xtools(from_utils)).
:- use_module(xtools(location_utils)).
:- use_module(xtools(module_files)).

%% module_to_import_db(F, A, M, CM, File)
%
% Predicate M:F/A used in File, in context CM

:- dynamic
    module_to_import_db/5.

file_to_module(Alias) :-
    file_to_module(Alias, []).

implementation_decl(dynamic).
implementation_decl(multifile).
implementation_decl(discontiguous).
implementation_decl(volatile).
implementation_decl(thread_local).
implementation_decl(clause(_)).

files_to_move(M, File, [File|FileL]) :-
    findall(MF, module_file(MF, File), MU),
    sort(MU, ML),
    member(M, ML),
    findall(IFile, file_includes(File, IFile), FileL).

file_includes(File, IFile) :-
    findall(Incl, source_file_property(File, includes(Incl, _)), IncD),
    remove_dups(IncD, IncL),
    member(Incl, IncL),
    ( IFile = Incl
    ; file_includes(Incl, IFile)
    ).

collect_def_files(M, PIL, FileL) :-
    findall(File,
	    ( member(F/A, PIL),
	      functor(H, F, A),
	      property_from((M:H)/_, _, From),
	      from_to_file(From, File)
	    ), FileU),
    sort(FileU, FileL).

file_to_module(Alias, OptionL0 ) :-
    select_option(module(M),         OptionL0, OptionL1, M),
    select_option(exclude(ExcludeL), OptionL1, OptionL2, []),
    select_option(addcl(AddL),       OptionL2, OptionL3, []),
    select_option(delcl(DelL),       OptionL3, _OptionL, []),
    absolute_file_name(Alias, File, [file_type(prolog), access(read)]),
    files_to_move(M, File, FileL),
    format('% from context ~a~n', [M]),
    collect_movable(M, FileL, ExcludeL, PIMo),
    collect_fixable(M, FileL, ExcludeL, PIFx, PIRn),
    directory_file_path(_, Name, File),
    file_name_extension(NewM, _, Name),
    add_qualification_head(M, PIFx, [module(M), files(FileL)]),
    add_qualification_decl(M, PIFx, [module(M), files(FileL)]),
    collect_def_files(M, PIRn, FileD),
    subtract(FileD, FileL, FileR),
    add_qualification_head(NewM, PIRn, [files(FileR)]),
    add_qualification_decl(NewM, PIRn, [files(FileR)]),
    ren_qualification_head(M, NewM, PIRn, [files(FileD)]),
    ren_qualification_decl(M, NewM, PIRn, [files(FileD)]),
    subtract(PIMo, PIFx, PIL1),
    subtract(PIL1, PIRn, PIL),
    report_dispersed_assertions(PIL1, FileL, M),
    collect_to_reexport(M, FileL, PIL1, ReexportL),
    collect_export_decl_files(M, ExFileL),
    del_modexp_decl(M, ReexportL),
    del_export_decl(M, ExFileL, ReexportL),
    del_export_decl(M, FileL, PIFx),
    add_modexp_decl(M, PIFx),
    add_modmeta_decl(M, PIFx),
    append([PIL1, PIRn, ExcludeL], ExU2),
    sort(ExU2, ExL2),
    phrase(( AddL,
	     collect_import_decls(M, FileL, ExL2),
	     collect_dynamic_decls(M, FileL),
	     collect_meta_decls(M, PIL)
	   ), MDL, []),
    findall(C, ( member(C, MDL),
		 replace_sentence(C, C, [max_changes(1),
					 changes(N),
					 file(File)]),
		 N = 0
	       ), CL),
    add_module_decl(CL, NewM, PIL1, File),
    decl_to_use_module(consult, M, PIL1, Alias, ReexportL),
    decl_to_use_module(include, M, PIL1, Alias, ReexportL),
    append(ExcludeL, PIFx, ExTL),
    add_use_module(M, FileL, Alias, ExTL),
    add_use_module_ex(M, FileL),
    del_use_module_ex(M, FileL),
    forall(member(C, DelL), replace_sentence(C, [], [file(File)])).

add_module_decl(CL, NewM, PIL1, File) :-
    replace_sentence([Term],
		     [Term@@(:- module('$BODY'((NewM, PIL2))))|CL],
		     ( Term = (:- export(ExS))
		     ->refactor_context(pattern, [(:- export(ExP))]),
		       sequence_list(ExS, ExL, []),
		       subtract(PIL1, ExL, PILD),
		       PIL2 = '$LISTB,NL'(['$PRIORITY'('$BODY'(ExP@@ExS),1200)|PILD])
		     ),
		     [max_tries(1), changes(N), file(File)]),
    ( N = 0
    ->replace_sentence([],
		       [(:- module('$BODY'((NewM,
					    '$LISTB,NL'(PIL1)))))|CL],
		       [max_changes(1), file(File)])
    ; true
    ).

collect_meta_decls(M, PIL, MDL, Tail) :-
    collect_meta_specs(M, PIL, SpecL),
    ( SpecL = []
    ->MDL = Tail
    ; MDL = [(:- meta_predicate('$LIST,NL'(SpecL)))|Tail]
    ).

collect_meta_specs(M, PIL, SpecL) :-
    findall(Spec, ( member(F/A, PIL),
		    functor(H, F, A),
		    \+ predicate_property(M:H, meta_predicate(Spec)),
		    inferred_meta_predicate(M:H, Spec)
		  ), SpecL).

add_modmeta_decl(M, PIFx) :-
    collect_meta_specs(M, PIFx, SpecL),
    ( SpecL \= [] ->
      replace_sentence((:- module(M, MEL)),
		       [(:- module(M, MEL)),
			(:- meta_predicate('$LIST,NL'(SpecL)))
		       ], [max_changes(1), module(M)])
    ; true
    ).

add_modexp_decl(M, PIFx) :-
    module_property(M, file(MFile)),
    replace_sentence((:- module(M, MEL)),
		     (:- module(M, NMExL)),
		     ( subtract(PIFx, MEL, NExL),
		       NExL \= [],
		       ( MEL = []
		       ->NMExL = '$LISTB,NL'(NExL)
		       ; append(MEL, '$LIST,NL'(NExL), NMExL)
		       )
		     ), [file(MFile)]).

%% collect_fixable(M, FileL, ExcludeL, PIM) is det
%
% Collect the predicates that preserves its implementation module, even if they
% have clauses in the file being modularized. That includes multifile predicates
% whose multifile declaration is outside FileL
%
collect_fixable(M, FileL, ExcludeL, PIM, PIR) :-
    findall(F/A,
	    ( current_predicate(M:F/A),
	      functor(H, F, A),
	      \+ memberchk(F/A, ExcludeL),
	      \+ predicate_property(M:H, imported_from(_)),
	      once(( implemented_in_file(F, A, M, InFile),
		     memberchk(InFile, FileL),
		     implemented_in_file(F, A, M, ExFile),
		     \+ memberchk(ExFile, FileL)
		   ))
	    ), PIU),
    sort(PIU, PIF),
    partition(preserve_module(M, FileL), PIF, PIM, PIR).

preserve_module(M, FileL, F/A) :-
    functor(H, F, A),
    %% predicate_property(M:H, multifile),
    once(( loc_declaration(H, M, multifile, MFrom),
	   from_to_file(MFrom, MFile),
	   \+ memberchk(MFile, FileL)
	 )).

add_qualification_head(M, PIM, OptionL) :-
    forall(member(F/A, PIM),
	   ( functor(H, F, A),
	     replace_head(H, M:H, OptionL)
	   )).

add_qualification_decl(M, PIM, OptionL) :-
    forall(( implementation_decl(DeclN),
	     DeclN \= clause(_)
	   ),
	   ( functor(Decl, DeclN, 1),
	     replace_term(F/A, M:F/A, ( atom(F),
					integer(A),
					memberchk(F/A, PIM)
				      ),
			  [sentence((:- Decl))|OptionL])
	   )).

ren_qualification_head(M, NewM, PIL, OptionL) :-
    forall(member(F/A, PIL),
	   ( functor(H, F, A),
	     replace_head(M:H, NewM:H, OptionL)
	   )).

ren_qualification_decl(M, NewM, PIL, OptionL) :-
    forall(( implementation_decl(DeclN),
	     DeclN \= clause(_)
	   ),
	   ( functor(Decl, DeclN, 1),
	     replace_term(M:F/A, NewM:F/A, ( atom(F),
					     integer(A),
					     memberchk(F/A, PIL)
					   ),
			  [sentence((:- Decl))|OptionL])
	   )).

add_use_module(M, FileL, Alias, ExcludeL) :-
    findall(CM-(F/A),
	    ( ( module_to_import_db(F, A, M, CM, _File),
		implemented_in_file(F, A, M, File),
		memberchk(File, FileL)
	      ; implem_to_export(FileL, F, A, M, CM)
	      ),
	      \+ memberchk(F/A, ExcludeL),
	      CM \= M
	    ),
	    CMPIU),
    sort(CMPIU, CMPIL),
    group_pairs_by_key(CMPIL, CMPIG),
    forall(member(CM-PIL, CMPIG),
	   add_use_module_cm(M, Alias, CM, PIL)).

add_use_module_cm(M, Alias, CM, PIL) :-
    module_property(CM, file(MFile)),
    replace_sentence((:- module(CM, MEL)),
		     [(:- module(CM, MEL)),
		      (:- use_module(Alias))],
		     [file(MFile)]),
    module_property(M, file(MainF)),
    replace_sentence((:- use_module(MainA, ExL)),
		     [],
		     ( absolute_file_name(MainA,
					  MainF1,
					  [file_type(prolog),
					   access(read)]),
		       MainF1=MainF,
		       subtract(ExL, PIL, ExL2),
		       ExL2 = []
		     ),
		     [module(CM)]),
    replace_sentence((:- use_module(MainA, ExL)),
		     (:- use_module(MainA, '$LISTB,NL'(ExL2))),
		     ( absolute_file_name(MainA,
					  MainF1,
					  [file_type(prolog),
					   access(read)]),
		       MainF1=MainF,
		       subtract(ExL, PIL, ExL2),
		       ExL2 \= []
		     ),
		     [module(CM)]).

declared_use_module(F, A, IM, M, EA, File) :-
    module_property(IM, file(ImplFile)),
    ( module_property(IM, exports(ExL)),
      loc_declaration(EA, M, use_module, From)
    ; loc_declaration(use_module(EA, Ex), M, use_module_2, From),
      ( is_list(Ex)
      ->ExL = Ex
      ; Ex = except(NotExL)
      ->module_property(IM, exports(ExA)),
	subtract(ExA, NotExL, ExL)
      )
    ),
    absolute_file_name(EA, EFile, [file_type(prolog), access(read)]),
    EFile = ImplFile,
    memberchk(F/A, ExL),
    from_to_file(From, File).

del_use_module_ex(M, FileL) :-
    replace_sentence((:- use_module(EA)),
		     [],
		     ( absolute_file_name(EA,
					  ImplementFile,
					  [file_type(prolog),
					   access(read)]),
		       module_property(IM, file(ImplementFile)),
		       \+ module_property(IM, exports([])),
		       \+ ( module_to_import_db(F, A, IM, M, File),
			    memberchk(File, FileL)
			  )
		     ),
		     [files(FileL)]),
    replace_sentence((:- use_module(EA, IL)),
		     [],
		     ( IL = [_|_],
		       absolute_file_name(EA,
					  ImplementFile,
					  [file_type(prolog),
					   access(read)]),
		       module_property(IM, file(ImplementFile)),
		       \+ module_property(IM, exports([])),
		       findall(F/A,
			       ( module_to_import_db(F, A, IM, M, File),
				 memberchk(File, FileL)
			       ), PIL),
		       intersection(IL, PIL, NIL),
		       NIL = []
		     ),
		     [files(FileL)]),
    replace_sentence((:- use_module(EA, IL)),
		     (:- use_module(EA, NIL)),
		     ( IL = [_|_],
		       absolute_file_name(EA,
					  ImplementFile,
					  [file_type(prolog),
					   access(read)]),
		       module_property(IM, file(ImplementFile)),
		       findall(F/A,
			       ( module_to_import_db(F, A, IM, M, File),
				 memberchk(File, FileL)
			       ), PIL),
		       intersection(IL, PIL, NIL),
		       NIL \= []
		     ),
		     [files(FileL)]).

add_use_module_ex(M, FileL) :-
    findall(ImportingFile-((IM:EA)-(F/A)),
	    [M, FileL, ImportingFile, IM, EA, F, A] +\
	    ( module_to_import_db(F, A, IM, M, ImportingFile),
	      IM \= M,
	      \+ module_file(IM, ImportingFile),
	      \+ declared_use_module(F, A, IM, M, _, ImportingFile),
	      declared_use_module(F, A, IM, M, EA, File),
	      memberchk(File, FileL),
	      absolute_file_name(EA,
				 ImplementFile,
				 [file_type(prolog),
				  access(read)]),
	      module_property(IM, file(ImplementFile))
	    ),
	    FileAliasPIU),
    sort(FileAliasPIU, FileAliasPIL),
    group_pairs_by_key(FileAliasPIL, FileAliasPIG),
    forall(member(ImFile-AliasPIL, FileAliasPIG),
	   add_use_module_ex_1(M, ImFile, AliasPIL)).

add_umexdecl_each(ImFile, M, AliasPIG, Decl) :-
    ( member((IM:Alias)-PIL, AliasPIG),
      module_property(IM, exports(ExL)),
      ( member(F/A, ExL),
	module_to_import_db(F, A, OM, M, ImFile),
	OM \= IM,
	functor(H, F, A),
	\+ predicate_property(IM:H, imported_from(OM))
      ->Decl = (:- use_module(Alias, PIL))
      ; Decl = (:- use_module(Alias))
      )
    ).

add_use_module_ex_1(M, ImFile, AliasPIL) :-
    group_pairs_by_key(AliasPIL, AliasPIG),
    findall(Decl, add_umexdecl_each(ImFile, M, AliasPIG, Decl), DeclL, Tail),
    ( DeclL == Tail
    ->true
    ; Tail = [],
      replace_sentence((:- module(ImM, Ex)),
		       [(:- module(ImM, Ex))|DeclL],
		       [max_changes(1), changes(C), file(ImFile)]),
      C \= 0
    ->true
    ; Term = (:- Decl),
      Tail = [Term],
      replace_sentence(Term, DeclL,
		       memberchk(Decl, [use_module(_), use_module(_,_)]),
		       [max_changes(1), changes(C), file(ImFile)]),
      C \= 0
    ->true
    ; Tail = [],
      replace_sentence([], DeclL, [max_changes(1), file(ImFile)])
    ).

collect_to_reexport(M, FileL, PIL, ReexportL) :-
    module_property(M, exports(EL1)),
    findall(PI,
	    ( PI=F/A,
	      member(PI, EL1),
	      functor(H, F, A),
	      loc_declaration(H, M, export, From),
	      from_to_file(From, FileX),
	      \+ memberchk(FileX, FileL)
	    ), EL),
    intersection(PIL, EL, ReexportL).

decl_to_use_module(Decl, M, PIL, Alias, ReexportL) :-
    findall(DFile, ( extra_location(Alias, M, Decl, DFrom),
		     from_to_file(DFrom, DFile)
		   ), DFileU),
    sort(DFileU, DFileL),
    ( ReexportL = []
    ->Into = (:- use_module(Alias))
    ; ( PIL = ReexportL
      ->Into = (:- reexport(Alias))
      ; subtract(PIL, ReexportL, ExportL),
	( ExportL = []
	->Into = (:- reexport(Alias, '$LISTB,NL'(ReexportL)))
	; Into = [(:- use_module(Alias)),
		  (:- reexport(Alias, '$LISTB,NL'(ReexportL)))]
	)
      )
    ),
    Patt =.. [Decl, Alias],
    replace_sentence((:- Patt), Into, [files(DFileL)]).

collect_export_decl_files(M, ExFileL) :-
    module_property(M, exports(Ex)),
    findall(ExFile, ( PI=F/A,
		      member(PI, Ex),
		      functor(H, F, A),
		      loc_declaration(H, M, export, From),
		      from_to_file(From, ExFile)
		    ), ExFileU),
    sort(ExFileU, ExFileL).

del_modexp_decl(M, DelExpDeclL) :-
    module_property(M, file(MFile)),
    replace_sentence((:- module(M, MEL)),
		     (:- module(M, '$LISTB,NL'(NL))),
		     ( subtract(MEL, DelExpDeclL, NL),
		       NL \= MEL
		     ), [file(MFile)]).

del_export_decl(M, ExFileL, DelExpDeclL) :-
    replace_sentence((:- export(ExS)),
		     Exp,
		     ( sequence_list(ExS, ExL, []),
		       subtract(ExL, DelExpDeclL, ExNL),
		       ExNL \= ExL,
		       ( ExNL = []
		       ->Exp = []
		       ; Exp = (:- export('$LIST,NL'(ExNL)))
		       )
		     ), [module(M), files(ExFileL)]),
    replace_sentence((:- M:export(ExS)),
		     MExp,
		     ( sequence_list(ExS, ExL, []),
		       subtract(ExL, DelExpDeclL, ExNL),
		       ( ExNL = []
		       ->MExp = []
			 ; MExp = (:- M:export('$LIST,NL'(ExNL)))
		       )
		     ), [files(ExFileL)]).

%% implem_to_export(FileL, F, A, M, CM)
%
% Predicate M:F/A implemented in FileL, is called outside FileL and therefore
% should be exported
%
implem_to_export(FileL, F, A, M, CM) :-
    ( loc_dynamic(H, M, dynamic(_, CM, _), FromD),
      from_to_file(FromD, FileD),
      \+ memberchk(FileD, FileL),
      ( loc_declaration(H, M, D, From),
	implementation_decl(D),
	from_to_file(From, File),
	memberchk(File, FileL)
      ->true
      ; loc_declaration(H, M, D, From),
	implementation_decl(D),
	from_to_file(From, FileE),
	\+ memberchk(FileE, FileL)
      ->fail
      ; loc_dynamic(H, M, dynamic(_, M, _), From),
	from_to_file(From, File),
	memberchk(File, FileL)
      ->true
      )
    ; loc_declaration(H, M, export, From),
      from_to_file(From, FileX),
      \+ memberchk(FileX, FileL),
      once(( property_from((M:H)/_, clause(_), PFrom),
	     from_to_file(PFrom, File),
	     memberchk(File, FileL)
	   )),
      M=CM
    ),
    functor(H, F, A).

report_dispersed_assertions(PIL, FileL, M) :-
    collect_dispersed_assertions(PIL, FileL, M, PIA),
    ( PIA \= []
    ->print_message(warning,
		    format('Assertions for ~w needs to be relocated', [PIA]))
    ; true
    ).

collect_dispersed_assertions(PIL, FileL, M, PIA) :-
    findall(F/A, ( member(F/A, PIL),
		   functor(H, F, A),
		   once(( implemented_in_file(F, A, M, File),
			  memberchk(File, FileL)
			)),
		   loc_declaration(H, M, assertion(_, _), FromD),
		   from_to_file(FromD, FileD),
		   \+ memberchk(FileD, FileL)
		 ), PIUA),
    sort(PIUA, PIA).

collect_movable(M, FileL, ExcludeL, PIL) :-
    OptionL = [source(false), trace_reference(_)],
    retractall(module_to_import_db(_, _, _, _, _)),
    prolog_walk_code([autoload(false),
		      source(false),
		      infer_meta_predicates(true)]),
    extra_walk_code(OptionL, collect_file_to_module, _, _),
    findall(F/A, ( module_to_import_db(F, A, M, _, IFile),
		   \+ memberchk(IFile, FileL),
		   implemented_in_file(F, A, M, File),
		   memberchk(File, FileL)
		 ), PIU, PIT),
    findall(F/A, implem_to_export(FileL, F, A, M, _), PIT),
    sort(PIU, PIS),
    subtract(PIS, ExcludeL, PIL).

implemented_in_file(F, A, M, File) :-
    functor(Goal, F, A),
    property_from((M:Goal)/_, Decl, PFrom),
    implementation_decl(Decl),
    from_to_file(PFrom, File).

collect_used_outside(M, FileL, ExcludeL, UOL, T) :-
    findall(EM-(F/A),
	    ( module_to_import_db(F, A, EM, M, File),
	      memberchk(File, FileL),
	      implemented_in_file(F, A, EM, IFile),
	      \+ memberchk(IFile, FileL),
	      \+ memberchk(F/A, ExcludeL)
	    ), UOL, T).

collect_decl_outside(M, FileL, ExcludeL, DOL, T) :-
    findall(EM-(F/A),
	    ( loc_dynamic(H, EM, dynamic(_, M, _), From),
	      from_to_file(From, File),
	      memberchk(File, FileL),
	      ( loc_declaration(H, EM, D, FromD),
		implementation_decl(D),
		from_to_file(FromD, FileD),
		memberchk(FileD, FileL)
	      ->fail
	      ; ( loc_declaration(H, EM, D, FromD),
		  implementation_decl(D)
		; loc_dynamic(H, EM, dynamic(Type, _, _), FromD),
		  memberchk(Type, [retract, def])
		),
		from_to_file(FromD, FileD),
		\+ memberchk(FileD, FileL)
	      ->true
	      ),
	      functor(H, F, A),
	      \+ memberchk(F/A, ExcludeL)
	    ), DOL, T).

collect_requires_dyn_decl(M, FileL, PID) :-
    findall(F/A,
	    ( loc_dynamic(H, M, dynamic(_, _, _), FromD),
	      from_to_file(FromD, FileD),
	      \+ memberchk(FileD, FileL),
	      ( loc_declaration(H, M, D, _),
		implementation_decl(D)
	      ->fail
	      ; loc_dynamic(H, M, dynamic(_, M, _), From),
		from_to_file(From, File),
		memberchk(File, FileL)
	      ->true
	      ),
	      functor(H, F, A)
	    ), PIUD),
    sort(PIUD, PID).

collect_dynamic_decls(M, FileL, DYL, Tail) :-
    collect_requires_dyn_decl(M, FileL, PID),
    ( PID = []
    ->DYL = Tail
    ; DYL = [(:- dynamic('$LIST,NL'(PID)))|Tail]
    ).

% collect_import_decls(+atm,+list(atm),+list,+list,-list,?list) is det.
%
collect_import_decls(M, FileL, ExcludeL, MDL, Tail) :-
    collect_used_outside(M, FileL, ExcludeL, UOL, DOL),
    collect_decl_outside(M, FileL, ExcludeL, DOL, []),
    sort(UOL, ML),
    group_pairs_by_key(ML, GL),
    findall((:- Decl),
	    ( member(EM-PEL, GL),
	      findall(PPI,
		      ( PPI=FF/AA,
			member(PPI, PEL),
			functor(HH, FF, AA),
			% \+ predicate_property(EM:HH, exported),
			\+ ( extra_location(HH, EM, export, EFrom),
			     from_to_file(EFrom, EFile),
			     ( memberchk(EFile, FileL)
			     ; module_property(M, file(EFile))
			     )
			   ),
			( predicate_property(EM:HH, D),
			  implementation_decl(D)
			->true
			; implemented_in_file(FF, AA, EM, _PFile)
			->true
			)
		      ), REL),
	      current_module(EM, EF),
	      smallest_alias(EF, EA),
	      \+ black_list_um(EA),
	      list_sequence(REL, RES),
	      ( EM=M, REL \= []
	      ->print_message(warning,
			      format("Back imports is a bad sign: ~w",
				     [(:- EM:export(RES))]))
	      ; true
	      ),
	      ( ( EM = M,
		  % PEL \= REL,
		  REL \= []
		->Decl = EM:export('$LIST,NL'(REL)) % Explicit exports --EMM
		; fail
		)
	      ;	\+ ( loc_declaration(EA, _, use_module, UMFrom),
		     from_to_file(UMFrom, UFile),
		     memberchk(UFile, FileL)
		   ),
		Decl = use_module(EA)
	      )
	    ), MDL, Tail).

black_list_um(swi(_)).		% Ignore internal SWI modules
black_list_um(library(dialect/_)).

collect_file_to_module(Callee, _Caller, From) :-
    record_location_meta(Callee, _, From, all_call_refs, cu_caller_hook).

cu_caller_hook(M:Head, CM, Type, Goal, _, From) :-
    nonvar(M),
    callable(Head),
    ( Type \= lit
    ->record_location(Head, M, dynamic(Type, CM, Goal), From)
    ; true
    ),
    record_calls_to(Head, M, CM, From).

record_calls_to(Head, M, CM, From) :-
    functor(Head, F, A),
    from_to_file(From, File),
    ( module_to_import_db(F, A, M, CM, File) -> true
    ; assertz(module_to_import_db(F, A, M, CM, File))
    ).
