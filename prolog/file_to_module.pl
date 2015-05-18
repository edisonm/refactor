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

:- use_module(library(normalize_head)).
:- use_module(library(extra_location)).
:- use_module(library(location_utils)).
:- use_module(library(from_utils)).
:- use_module(library(infer_alias)).
:- use_module(library(implementation_module)).
:- use_module(library(list_sequence)).
:- use_module(library(sequence_list)).
:- use_module(library(module_files)).
:- use_module(library(audit/audit_codewalk)).

:- dynamic
    module_to_export_db/4,
    module_to_import_db/3.

file_to_module(Alias) :-
    file_to_module(Alias, _).

implementation_decl(dynamic).
implementation_decl(discontiguous).
implementation_decl(volatile).
implementation_decl(thread_local).
implementation_decl(clause(_)).

collect_not_exported(M, File, PIL, PIEx) :-
    findall(PI,
	    ( PI=F/A,
	      member(PI, PIL),
	      functor(H, F, A),
	      loc_declaration(H, M, export, From),
	      from_to_file(From, File)
	    ), PIEx).

file_to_module(Alias, Module) :-
    absolute_file_name(Alias, File, [file_type(prolog), access(read)]),
    file_modules(File, ModuleL),
    member(Module, ModuleL),
    format('% from context ~a~n', [Module]),
    file_to_module_(File, Module:_, Base, PIL, PIM, MDL),
    collect_not_exported(Module, File, PIL, PIEx),
    replace_sentence([], [(:- module(Base, PIEx))|MDL], [file(File)]),
    forall(member(F/A, PIM),
	   ( functor(H, F, A),
	     findall(DFile,
		     ( property_from(Module:H, _, PFrom),
		       from_to_file(PFrom, DFile),
		       DFile \= File
		     ), FileL),
	     replace_head(H, Base:H, [module(Module), aliases(FileL)])
	   )),
    decl_to_use_module(consult, Module, File, PIL, Alias),
    decl_to_use_module(include, Module, File, PIL, Alias),
    add_use_module(Module, File, Alias).

add_use_module(M, File, Alias) :-
    findall(CM-(F/A),
	    ( ( module_to_export_db(F, A, M, CM)
	      ; implem_to_export(File, F, A, M, CM)
	      ),
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
		     [alias(MFile)]),
    module_property(M, file(MainF)),
    replace_sentence((:- use_module(MainA, ExL)),
		     (:- use_module(MainA, ExL2)),
		     ( absolute_file_name(MainA,
					  MainF1,
					  [file_type(prolog),
					   access(read)]),
		       MainF1=MainF,
		       subtract(ExL, PIL, ExL2)
		     ),
		     [module(CM)]).

decl_to_use_module(Decl, M, File, PIL, Alias) :-
    findall(DFile, ( extra_location(Alias, M, Decl, DFrom),
		     from_to_file(DFrom, DFile)
		   ), DFileU),
    sort(DFileU, DFileL),
    Patt =.. [Decl, Alias],
    module_property(M, exports(EL1)),
    findall(PI, ( PI=F/A,
		  member(PI, EL1),
		  functor(H, F, A),
		  loc_declaration(H, M, export, From),
		  from_to_file(From, FileX),
		  FileX \= File
		), EL),
    intersection(PIL, EL, ReexportL),
    ( ReexportL = []
    ->Into = (:- use_module(Alias))
    ; module_property(M, file(MFile)),
      replace_sentence((:- module(M, MEL)), (:- module(M, '$LISTB,NL'(NL))),
			( subtract(MEL, ReexportL, NL),
			  NL \= MEL
			), [alias(MFile)]),
      ( PIL = ReexportL
      ->Into = (:- reexport(Alias))
      ; subtract(PIL, ReexportL, ExportL),
	( ExportL = []
	->Into = (:- reexport(Alias, ReexportL))
	; Into = [(:- use_module(Alias)),
		  (:- reexport(Alias, ReexportL))]
	)
      )
    ),
    replace_sentence((:- Patt), Into, [aliases(DFileL)]).

implem_to_export(File, F, A, M, CM) :-
    ( loc_dynamic(H, M, dynamic(_, CM, _), FromD),
      from_to_file(FromD, FileD),
      FileD \= File,
      ( loc_declaration(H, M, D, From),
	implementation_decl(D),
	from_to_file(From, File)
      ->true
      ; loc_declaration(H, M, D, From),
	implementation_decl(D),
	from_to_file(From, FileE),
	FileE \= File
      ->fail
      ; loc_dynamic(H, M, dynamic(_, M, _), From),
	from_to_file(From, File)
      ->true
      )
    ; loc_declaration(H, M, export, From),
      from_to_file(From, FileX),
      FileX \= File,
      once(( property_from((M:H)/_, clause(_), PFrom),
	     from_to_file(PFrom, File)
	   )),
      M=CM
    ),
    functor(H, F, A).

file_to_module_(File, Ref, Base, PIL, PIM, MDL) :-
    directory_file_path(_, Name, File),
    file_name_extension(Base, _, Name),
    normalize_head(Ref, M:H),
    OptionL = [source(false),
	       trace_reference(_:H)],
    retractall(module_to_export_db(_, _, _, _)),
    retractall(module_to_import_db(_, _, _)),
    audit_walk_code(OptionL, collect_dynamic_locations(M, File), _, _),
    audit_walk_code(OptionL, collect_file_to_module(M, File), _, _),
    findall(F/A, ( module_to_export_db(F, A, M, _)
		 ; implem_to_export(File, F, A, M,_)
		 ), PIU),
    sort(PIU, PIL),
    findall(F/A, ( member(F/A, PIL),
		   functor(H, F, A),
		   loc_declaration(H, M, assertion(_, _), FromD),
		   once(( property_from((M:H)/_, clause(_), PFrom),
			  from_to_file(PFrom, File)
			)),
		   from_to_file(FromD, FileD),
		   FileD \= File
		 ), PIUA),
    sort(PIUA, PIA),
    ( PIA \= []
    ->print_message(warning,
		    format('Assertions for ~w needs to be relocated', [PIA]))
    ; true
    ),
    findall(EM-(F/A), ( retract(module_to_import_db(F, A, EM)),
			\+ memberchk(F/A, PIL)), MU, MD),
    findall(EM-(F/A), ( loc_dynamic(H, EM, dynamic(_, M, _), From),
			from_to_file(From, File),
			( loc_declaration(H, EM, D, FromD),
			  implementation_decl(D),
			  from_to_file(FromD, File)
			->fail
			; ( loc_declaration(H, EM, D, FromD),
			    implementation_decl(D)
			  ; loc_dynamic(H, EM, dynamic(Type, _, _), FromD),
			    memberchk(Type, [retract, def])
			  ),
			  from_to_file(FromD, FileD),
			  FileD \= File
			->true
			),
			functor(H, F, A)
		      ), MD),
    sort(MU, ML),
    group_pairs_by_key(ML, GL),
    findall(F/A, ( loc_dynamic(H, M, dynamic(_, _, _), FromD),
		   from_to_file(FromD, FileD),
		   FileD \= File,
		   ( loc_declaration(H, M, D, _),
		     implementation_decl(D)
		   ->fail
		   ; loc_dynamic(H, M, dynamic(_, M, _), From),
		     from_to_file(From, File)
		   ->true
		   ),
		   functor(H, F, A)
		 ), PIUD),
    sort(PIUD, PID),
    ( PID = []
    ->DYL = []
    ; list_sequence(PID, PIS),
      DYL = [(:- dynamic(PIS))]
    ),
    findall(F/A, ( member(F/A, PIL),
		   functor(H, F, A),
		   findall(DFile,
			   ( property_from((M:H)/_, clause(_), PFrom),
			     from_to_file(PFrom, DFile)
			     % DFile \= File
			   ), DFileU),
		   memberchk(File, DFileU),
		   sort(DFileU, DFileL),
		   DFileL = [_, _|_]
		   % \+ predicate_property(M:H, multifile)
		 ), PIUM),
    sort(PIUM, PIM),
    ( PIM = []
    ->MDT = DYL
    ; MDT = [(:- multifile('$LIST,NL'(PIM)))|DYL]
    ),
    findall((:- Decl),
	    ( member(EM-PEL, GL),
	      findall(PPI,
		      ( PPI=FF/AA,
			member(PPI, PEL),
			functor(HH, FF, AA),
			\+ predicate_property(EM:HH, exported),
			( predicate_property(EM:HH, D),
			  implementation_decl(D)
			->true
			; property_from((EM:HH)/_, clause(_), PFrom),
			  from_to_file(PFrom, _PFile)
			->true
			)
		      ), REL),
	      current_module(EM, EF),
	      smallest_alias(EF, EA),
	      \+ black_list_um(EA),
	      add_export_declarations_to_file(REL, File, EM),
	      \+ ( loc_declaration(EA, _, use_module, UMFrom),
		   from_to_file(UMFrom, File)
		 ),
	      Decl=use_module(EA)
	    ), MDL, MDT).

:- dynamic
    replaced/0.

add_export_declarations_to_file(REL, MFile, M) :-
    findall(File-PI,
	    ( PI = F/A,
	      member(PI, REL),
	      functor(H, F, A),
	      property_from(M:H, _, From),
	      from_to_file(From, File),
	      MFile \= File
	    ), FilePIU),
    sort(FilePIU, FilePIL),
    group_pairs_by_key(FilePIL, FilePIG),
    forall(member(File-PIL, FilePIG),
	   ( retractall(replaced),
	     ( module_property(M, file(File))
	     ->replace_sentence((:- module(M, L0 )),
				(:- module(M, L )),
				append(L0, PIL, L),
				[alias(File)])
	     ; replace_sentence((:- export(S)),
				(:- export('$LIST,NL'([S|PIL]))),
				( \+ replaced,
				  assertz(replaced)
				),
				[alias(File)]),
	       ( \+ replaced
	       ->replace_sentence([], (:- export('$LIST,NL'(PIL))), [alias(File)])
	       ; true
	       )
	     )
	   )).

black_list_um(swi(_)).		% Ignore internal SWI modules
black_list_um(library(dialect/_)).

collect_dynamic_locations(M, File, MGoal, _, From) :-
    nonvar(MGoal),
    from_to_file(From, File),	% match the file
    record_location_dynamic(MGoal, M, From).

collect_file_to_module(M, File, MGoal, _Caller, From) :-
    MGoal = CM:Goal,
    % \+ predicate_property(MGoal, built_in),
    from_to_file(From, FromFile),
    implementation_module(MGoal, IM),
    ( FromFile \= File,
      IM = M
    ->once(( property_from((IM:Goal)/_, Decl, PFrom),
	     implementation_decl(Decl),
	     from_to_file(PFrom, File)
	   )),
	   %  predicate_property(MGoal, file(File))
	   % ; extra_location(Goal, IM, Decl, EFrom),
	   %   from_to_file(EFrom, File),
	   %   memberchk(Decl, [dynamic, thread_local, multifile, discontiguous])
	   % )),
      functor(Goal, F, A),
      ( module_to_export_db(F, A, IM, CM) -> true
      ; assertz(module_to_export_db(F, A, IM, CM))
      )
    ; FromFile = File,
      property_from(IM:Goal, Decl, PFrom),
      Decl \= (export),
      from_to_file(PFrom, ImplFile),
      ImplFile \= File,
      CM = M
    ->functor(Goal, F, A),
      ( module_to_import_db(F, A, IM) -> true
      ; assertz(module_to_import_db(F, A, IM))
      )
    ; true
    ).
