/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(file_to_module, [file_to_module/1, file_to_module/2]).

:- use_module(library(prolog_metainference)).
:- use_module(library(clambda)).
:- use_module(library(infer_alias)).
:- use_module(library(list_sequence)).
:- use_module(library(sequence_list)).
:- use_module(library(extra_codewalk)).
:- use_module(library(extra_location)).
:- use_module(library(from_utils)).
:- use_module(library(location_utils)).
:- use_module(library(module_files)).
:- use_module(library(pretty_decl)).
:- use_module(library(ref_replace)).

%!  module_to_import_db(F, A, M, CM, File)
%
%   Predicate M:F/A used in File, in context CM

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
    distinct(Incl, source_file_property(File, includes(Incl, _))),
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
    add_declarations(MDL, File),
    decl_to_use_module(consult, M, PIL1, Alias, ReexportL),
    decl_to_use_module(include, M, PIL1, Alias, ReexportL),
    append(ExcludeL, PIFx, ExTL),
    add_use_module(M, FileL, Alias, AddL, ExTL),
    add_use_module_ex(M, DelL, FileL),
    del_use_module_ex(M, FileL),
    add_module_decl(NewM, PIL1, File),
    forall(member(C, DelL), replace_sentence(C, [], [files(FileL)])).

add_module_decl(NewM, PIL1, File) :-
    pretty_decl((:- module(NewM, PIL2)), PDecl2),
    replace_sentence(Term, PDecl2,
                     ( Term = (:- export(ExS))
                     ->sequence_list(ExS, ExL, []),
                       subtract(PIL1, ExL, PILD),
                       append(ExL, PILD, PIL2)
                     ),
                     [max_tries(1), changes(N), file(File)]),
    ( N = 0
    ->pretty_decl((:- module(NewM, PIL1)), PDecl1),
      replace_sentence([],
                       [PDecl1],
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
    pretty_decl((:- module(M, NMExL)), PDecl),
    replace_sentence((:- module(M, MEL)),
                     PDecl,
                     ( subtract(PIFx, MEL, NExL),
                       NExL \= [],
                       ( MEL = []
                       ->pretty_decl((:- module(M, PIFx)), PDecl)
                       ; append(MEL, '$LIST,NL'(NExL,'$1'+1), NMExL),
                         PDecl = (:- $@(module('$POS'('$1', M),
                                               '$NL'(NMExL$@MEL, '$1'))))
                       )
                     ), [file(MFile)]).

%!  collect_fixable(M, FileL, ExcludeL, PIM) is det
%
%   Collect the predicates that preserves its implementation module, even if
%   they have clauses in the file being modularized. That includes multifile
%   predicates whose multifile declaration is outside FileL
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

add_use_module(M, FileL, Alias, AddL, ExcludeL) :-
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
           add_use_module_cm(M, Alias, AddL, CM, PIL)).

add_use_module_cm(M, Alias, AddL, CM, PIL) :-
    module_property(CM, file(MFile)),
    reverse([(:- module(CM, _))|AddL], TopCL),
    once(( member(Term, TopCL),
           replace_sentence(Term,
                            [Term,
                             (:- use_module(Alias))],
                            [max_changes(1), changes(C), file(MFile)]),
           C \= 0
         )),
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
    pretty_decl(:- use_module(MainA, ExL2), PDecl),
    replace_sentence((:- use_module(MainA, ExL)), PDecl,
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
    pretty_decl((:- use_module(EA, NIL)), PDecl),
    replace_sentence((:- use_module(EA, IL)), PDecl,
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

add_use_module_ex(M, DelL, FileL) :-
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
           add_use_module_ex_1(M, DelL, ImFile, AliasPIL)).

add_umexdecl_each(ImFile, M, AliasPIG, (:- Decl)) :-
    member((IM:Alias)-PIL, AliasPIG),
    get_use_module_decl(ImFile, M, IM, Alias, PIL, Decl).

add_declarations(DeclL, ImFile) :-
    ( DeclL = []
    ->true
    ; findall(NDecl,
              ( member(NDecl, DeclL),
                ( NDecl = (:- use_module(A))
                ->( replace_sentence(NDecl, (:- use_module(A)),
                                     [max_changes(1), changes(C), file(ImFile)]),
                    C \= 0
                  ->fail
                  ; replace_sentence((:- use_module(A, _)), (:- use_module(A)),
                                     [max_changes(1), changes(0), file(ImFile)])
                  )
                ; NDecl = (:- use_module(A, L1))
                ->( replace_sentence((:- use_module(A)), (:- use_module(A)),
                                     [max_changes(1), changes(C), file(ImFile)]),
                    C \= 0
                  ->fail
                  ; pretty_decl((:- use_module(A, L)), PDecl),
                    replace_sentence((:- use_module(A, L2)), PDecl,
                                     ( is_list(L2),
                                       subtract(L1, L2, L3),
                                       append(L2, '$LIST,NL'(L3,'$1'(1)+1), L)
                                     ),
                                     [max_changes(1), changes(0), file(ImFile)])
                  )
                ; true
                )
              ), DeclL2),
      ( DeclL2 = []
      ->true
      ; foldl(pretty_decl, DeclL2, DeclL3, 1, _),
        ( Term = (:- Decl),
          append(DeclL3, [(:- Decl)], DeclL4),
          replace_sentence(Term, DeclL4,
                           memberchk(Decl, [use_module(_), use_module(_,_)]),
                           [max_changes(1), changes(C), file(ImFile)]),
          C \= 0
        ->true
        ; ( replace_sentence((:- module(ImM, Ex)),
                             [(:- module(ImM, Ex))|DeclL3],
                             [max_changes(1), changes(C), file(ImFile)]),
            C \= 0
          ->true
          ; replace_sentence([], DeclL3, [max_changes(1), file(ImFile)])
          )
        )
      )
    ).

add_use_module_ex_1(M, DelL, ImFile, AliasPIL) :-
    group_pairs_by_key(AliasPIL, AliasPIG),
    findall(Decl, ( add_umexdecl_each(ImFile, M, AliasPIG, Decl),
                    \+ memberchk(Decl, DelL)
                  ), DeclL),
    add_declarations(DeclL, ImFile).

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

alias_location(Alias, M, Decl, IAlias-File) :-
    ( extra_location(Alias, M, Decl, From),
      from_to_file(From, File)
    ->IAlias = Alias
    ; absolute_file_name(Alias, IFile, [file_type(prolog), access(read)]),
      extra_location(IAlias, M, Decl, From),
      from_to_file(From, File),
      absolute_file_name(IAlias, IFile,
                         [file_type(prolog), access(read),
                          file_errors(fail), relative_to(File)]),
      !
    ).

decl_to_use_module(Decl, M, PIL, Alias, ReexportL) :-
    findall(DFile, alias_location(Alias, M, Decl, DFile), DAFileU),
    sort(DAFileU, DAFileL),
    ( ReexportL = []
    ->Into = (:- use_module(Alias))
    ; ( PIL = ReexportL
      ->Into = (:- reexport(Alias))
      ; subtract(PIL, ReexportL, ExportL),
        pretty_decl((:- reexport(Alias, ReexportL)), PDecl, 1),
        ( ExportL = []
        ->Into = PDecl
        ; Into = [(:- use_module(Alias)), PDecl]
        )
      )
    ),
    Patt =.. [Decl, IAlias],
    group_pairs_by_key(DAFileL, DAFileG),
    forall(member(IAlias-DFileL, DAFileG),
           replace_sentence((:- Patt), Into, [files(DFileL)])).

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
    pretty_decl((:- module(M, NL)), PDecl),
    replace_sentence((:- module(M, MEL)), PDecl,
                     ( subtract(MEL, DelExpDeclL, NL),
                       NL \= MEL
                     ), [file(MFile)]).

del_export_decl(M, ExFileL, DelExpDeclL) :-
    pretty_decl((:- export(ExNL)), ExDecl),
    replace_sentence((:- export(ExS)), Exp,
                     ( sequence_list(ExS, ExL, []),
                       subtract(ExL, DelExpDeclL, ExNL),
                       ExNL \= ExL,
                       ( ExNL = []
                       ->Exp = []
                       ; Exp = ExDecl
                       )
                     ), [module(M), files(ExFileL)]),
    pretty_decl((:- M:export(ExNL)), MExDecl),
    replace_sentence((:- M:export(ExS)),
                     MExp,
                     ( sequence_list(ExS, ExL, []),
                       subtract(ExL, DelExpDeclL, ExNL),
                       ExNL \= ExL,
                       ( ExNL = []
                       ->MExp = []
                       ; MExp = MExDecl
                       )
                     ), [files(ExFileL)]).

%!  implem_to_export(FileL, F, A, M, CM)
%
%  Predicate M:F/A implemented in FileL, is called outside FileL and therefore
%  should be exported
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
    extra_walk_code([on_trace(collect_file_to_module)|OptionL]),
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
                  memberchk(Type, [retract, def, dec])
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
              library_alias(EF, EA),
              \+ black_list_um(EA),
              ( EM=M, REL \= []
              ->list_sequence(REL, RES),
                print_message(warning,
                              format("Back imports is a bad sign: ~w",
                                     [(:- EM:export(RES))]))
              ; true
              ),
              ( ( EM = M,
                  % PEL \= REL,
                  REL \= []
                ->Decl = EM:export(REL) % Explicit exports --EMM
                ; fail
                )
              ; \+ ( loc_declaration(EA, _, use_module, UMFrom),
                     from_to_file(UMFrom, UFile),
                     memberchk(UFile, FileL)
                   ),
                get_use_module_decl('', M, EM, EA, REL, Decl)

              )
            ), MDL, Tail).

get_use_module_decl(ImFile, M, IM, EA, REL, Decl) :-
    ( IM \= M,
      module_property(IM, exports(ExL)),
      member(F/A, ExL),
      functor(H, F, A),
      predicate_property(M:H, defined),
      ( module_to_import_db(F, A, OM, M, ImFile),
        OM \= IM,
        \+ predicate_property(IM:H, imported_from(OM))
      ; \+ ( predicate_property(M:H, imported_from(MM)),
             ( MM = IM
             ; predicate_property(IM:H, imported_from(MM)),
               predicate_property(IM:H, exported)
             )
           )
      )
    ->Decl = use_module(EA, REL)
    ; Decl = use_module(EA)
    ).

black_list_um(swi(_)).          % Ignore internal SWI modules
black_list_um(library(dialect/_)).

:- public collect_file_to_module/3.
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
