/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2022, Process Design Center, Breda, The Netherlands.
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

:- module(move_pred,
          [ move_predicates/4,
            update_move_predicates_db/3
          ]).

:- use_module(library(assertions)).
:- use_module(library(list_sequence)).
:- use_module(library(module_links)).
:- use_module(library(sequence_list)).
:- use_module(library(pretty_decl)).

:- multifile
        cond_move_pred_hook/4,
        move_predicates_hook/6.

:- dynamic
        target_file_module/2.

depends_of(AH, AM, H, M, CM, N) :-
    depends_of_db(AH, AM, H, M, CM, N).

module_from_file(File, M) :-
    ( module_property(M, file(File))
    ->true
    ; target_file_module(File, M)
    ).

defined_predicate(M:H) :-
    once(( depends_of_db(H, M, _, _, _, 1)
         ; depends_of_db(_, _, H, M, _, 1)
         % ; predicate_property(M:H, defined)
         )).

cond_move_pred(Term, _, _, _, _) :-
    var(Term),
    !,
    fail.
cond_move_pred(Term, MSource, _FTarget, PredList, Into) :-
    cond_move_pred_hook(Term, MSource, PredList, Into),
    !.
cond_move_pred(Term, M, _, PredList, []) :-
    memberchk(Term,
              [ G --> _
              ]),
    !,
    strip_module(M:G, CM, P),
    functor(P, F, D),
    A is D + 2,
    memberchk(CM:F/A, PredList).
cond_move_pred(Term, M, _, PredList, []) :-
    memberchk(Term,
              [ (H :- _)
              ]),
    !,
    strip_module(M:H, CM, P),
    nonvar(P),
    functor(P, F, A),
    memberchk(CM:F/A, PredList).
cond_move_pred((:- module(_, _)), _, _, _, _) :-
    !,
    fail.
cond_move_pred((:- include(Alias)), _, _, _, (:- include(Alias))) :-
    % We asume you don't use include to add predicates... do you???
    !.
cond_move_pred((:- Decl), MSource, _, PredList, Into) :-
    Decl =.. [DeclF, Sequence],
    memberchk(DeclF, [(meta_predicate), (multifile), (discontiguous),
                      (dynamic), (thread_local), (public), (export)]),
    !,
    sequence_list(Sequence, List, []),
    findall(Elem,
            ( member(Elem, List),
              \+ ( strip_module(MSource:Elem, M, H),
                   ( H = F/A
                   ->true
                   ; H = F//D
                   ->A is D + 2
                   ; functor(H, F, A)
                   ),
                   memberchk(M:F/A, PredList)
                 )
            ), Diff),
    Diff \= List,
    ( Diff = []
    ->Into = []
    ; list_sequence(Diff, ISeq),
      Decl2 =.. [DeclF, ISeq],
      Into = (:- Decl2)
    ).
cond_move_pred((:- use_module(Alias)), MSource, FTarget, PredList, Into) :-
    !,
    absolute_file_name(Alias, File, [file_errors(fail), access(exist), file_type(prolog)]),
    File \= FTarget,
    module_from_file(File, M),
    once(( depends_of(H2, M2, _, M, MSource, 1),
           functor(H2, F2, A2),
           memberchk(M2:F2/A2, PredList)
         )),
    ( depends_of(H1, M1, _, M, MSource, 1),
      functor(H1, F1, A1),
      \+ memberchk(M1:F1/A1, PredList)
    ->Into = (:- use_module(Alias))
    ; Into = []
    ).
cond_move_pred((:- use_module(Alias, ExL1)), MSource, FTarget, PredList, Into) :-
    !,
    absolute_file_name(Alias, File, [file_errors(fail), access(exist), file_type(prolog)]),
    File \= FTarget,
    module_from_file(File, M),
    \+ \+ ( member(F/A, ExL1),
            functor(H, F, A),
            once(( depends_of(H2, M2, H, M, MSource, _),
                   functor(H2, F2, A2),
                   memberchk(M2:F2/A2, PredList)
                 ))
          ),
    findall(F/A,
            ( member(F/A, ExL1),
              functor(H, F, A),
              once(( depends_of(H1, M1, H, M, MSource, _),
                     functor(H1, F1, A1),
                     \+ memberchk(M1:F1/A1, PredList)
                   ))
            ), ExL),
    ( ExL = []
    ->Into = []
    ; Into = (:- use_module(Alias, ExL))
    ).
cond_move_pred(H, M, _, PredList, []) :-  % This must be the last clause
    functor(H, F, A),
    memberchk(M:F/A, PredList).

add_exports_module(MSource, Target, PredList, Options) :-
    pretty_decl((:- module(MTarget, L)), Decl),
    replace_sentence((:- module(MTarget, L1)), Decl,
                     ( findall(F/A,
                               ( member(MSource:F/A, PredList),
                                 functor(H, F, A),
                                 once(( depends_of(H2, M2, H, MSource, CM, 1),
                                        CM \= MTarget,
                                        functor(H2, F2, A2),
                                        \+ memberchk(M2:F2/A2, PredList)
                                      ))
                               ), U, L1),
                       sort(U, L)
                     ),
                     [file(Target)|Options]).

cleanup_use_module(MSource, PredList, Options) :-
    replace_sentence((:- use_module(Alias, ExL1)), (:- use_module(Alias, ExL)),
                     ( absolute_file_name(Alias, File, [access(exist), file_type(prolog)]),
                       module_from_file(File, M),
                       findall(F/A,
                               ( member(F/A, ExL1),
                                 functor(H, F, A),
                                 once(( depends_of(H2, M2, H, M, MSource, 1),
                                        functor(H2, F2, A2),
                                        memberchk(M2:F2/A2, PredList)
                                      ))
                               ), ExL)
                     ), Options).

cleanup_declaration(MSource, MTarget, PredList, Decl, Into) :-
    Decl =.. [DeclF, Sequence],
    memberchk(DeclF, [(meta_predicate), (multifile), (discontiguous),
                      (dynamic), (thread_local), (public), (export)]),
    sequence_list(Sequence, List, []),
    findall(Elem,
            ( member(Elem, List),
              ( Elem = M:P
              ->M1 = M,
                M3 = M,
                ( P = F/A
                ->true
                ; functor(P, F, A)
                )
              ; M1 = MSource,
                M3 = MTarget,
                ( Elem = F/A
                ->true
                ; functor(Elem, F, A)
                )
              ),
              functor(H, F, A),
              ( defined_predicate(M3:H)
              ->true
              ; defined_predicate(M1:H)
              ->memberchk(M1:F/A, PredList)
              )
            ), List2),
    List \= List2,
    ( List2 = []
    ->Into = []
    ; list_sequence(List2, Sequence2),
      Decl2 =.. [DeclF, Sequence2],
      Into = (:- Decl2)
    ).

cleanup_declarations(MSource, MTarget, PredList, Options) :-
    replace_sentence((:- Decl), Into,
                     cleanup_declaration(MSource, MTarget, PredList, Decl, Into),
                     Options).

:- dynamic
    declared_db/3.

add_new_use_module(MSource, MTarget, Source, Target, PredList, Options) :-
    findall(CM,
            ( depends_of_db(_, _, H, MSource, CM, 1),
              CM \= MSource,
              CM \= MTarget,
              functor(H, F, A),
              memberchk(MSource:F/A, PredList),
              \+ ( depends_of_db(_, _, H, MSource, CM, 1),
                   CM \= MSource,
                   functor(H, F, A),
                   \+ memberchk(MSource:F/A, PredList)
                 )
            ), CMU),
    sort(CMU, CML),
    forall(member(CM, CML), add_use_mod(Target, [module(CM), below(Source)|Options])),
    replace_sentence((:- use_module(Source)), [],
                     \+ ( depends_of_db(_, _, H, MSource, CM, 1),
                          CM \= MSource,
                          functor(H, F, A),
                          \+ memberchk(MSource:F/A, PredList)
                        ), [module(CM), modules([MSource|CML])|Options]),
    del_dup_use_module([modules([MSource|CML])|Options]),
    del_dup_use_module([files([Target])|Options]).

del_dup_use_module(Options) :-
    replace_sentence((:- Decl), [],
                     ( memberchk(Decl, [include(_), use_module(_), use_module(_, _)]),
                       ( declared_db(CM, File, Decl)
                       ->true
                       ; assertz(declared_db(CM, File, Decl)),
                         fail
                       )
                     ), [file(File), module(CM)|Options]),
    retractall(declared_db(_, _, _)).

extern_dependency(target, H2, M2, H, M) :- depends_of_db(H2, M2, H, M, M, 1).
extern_dependency(source, H2, M,  H, M) :- depends_of_db(H,  M, H2, M, M, 1). % for source, M = M2

add_use_mod(Type, M, Alias, PredList, Options) :-
    ( member(M:F/A, PredList),
      functor(H, F, A),
      extern_dependency(Type, H2, M2, H, M),
      functor(H2, F2, A2),
      \+ memberchk(M2:F2/A2, PredList)
    ->add_use_mod(Alias, Options)
    ; true
    ).

add_use_mod(Alias, Options1) :-
    Options = [max_changes(1), changes(C)|Options1],
    once(( ( ( option(below(Prev), Options),
               member(Decl, [(:- use_module(Prev)), (:- use_module(Prev, _))]),
               replace_sentence(Decl, [Decl, (:- use_module(Alias))], Options)
             ; replace_sentence([(:- use_module(Prev)), Next],
                                [(:- use_module(Prev)), (:- use_module(Alias)), Next],
                                \+ memberchk(Next, [(:- use_module(_)),(:- use_module(_, _))]),
                                Options)
             )
           ; member(Term, [(:- include(_)), (:- module(_, _))]),
             replace_sentence(Term, [Term, (:- use_module(Alias))], Options)
           ),
           C \= 0
         )).

normalize_pred_id(M1, PI, M:F/A) :-
    ( PI = M:F/A
    ->true
    ; PI = F/A,
      M = M1
    ).

update_db(PredList, MSource, MTarget, FTarget) :-
    assertz(target_file_module(FTarget, MTarget)),
    forall(( member(M:F/A, PredList),
             functor(H, F, A)
           ),
           ( forall(retract(depends_of_db(AH, AM, H, M, CM, N)),
                    ( ( M = MSource
                      ->TM = MTarget
                      ; TM = M
                      ),
                      assertz(depends_of_db(AH, AM, H, TM, CM, N))
                    )),
             forall(retract(depends_of_db(H, M, TH, TM, MSource, N)),
                    ( ( M = MSource
                      ->AM = MTarget
                      ; AM = M
                      ),
                      assertz(depends_of_db(H, AM, TH, TM, MTarget, N))
                    ))
           )).

update_move_predicates_db(PredList1, Source, Target) :-
    process_args(PredList1, Source, Target, PredList, MSource, MTarget, FTarget),
    update_db(PredList, MSource, MTarget, FTarget).

process_args(PredList1, Source, Target, PredList, MSource, MTarget, FTarget) :-
    absolute_file_name(Source, FSource, [file_type(prolog), access(exist)]),
    module_from_file(FSource, MSource), % This should exist
    absolute_file_name(Target, FTarget, [file_type(prolog)]),
    maplist(normalize_pred_id(MSource), PredList1, PredList),
    ( module_from_file(FTarget, MTarget)
    ->true
    ; file_name_extension(BaseDir, _, FTarget),
      directory_file_path(_, MTarget, BaseDir)
    ).

move_predicates(PredList1, Source, Target, Options) :-
    process_args(PredList1, Source, Target, PredList, MSource, MTarget, FTarget),
    ( exists_file(FTarget)
    ->true
    ; tell(FTarget),
      told
    ),
    ( size_file(FTarget, 0 )
    ->replace_sentence([], (:- module(MTarget, [])), [file(Target)|Options])
    ; true
    ),
    pretty_decl((:- module(M, L)), Decl),
    replace_sentence((:- module(M, L1)), Decl,
                     ( MSource = TM,
                       findall(TF/TA,
                               ( member(TF/TA, L1),
                                 \+ memberchk(TM:TF/TA, PredList)
                               ; member(TM:AF/AA, PredList),
                                 once(( functor(AH, AF, AA),
                                        depends_of_db(AH, TM, TH, TM, TM, 1),
                                        functor(TH, TF, TA),
                                        \+ memberchk(TM:TF/TA, PredList)
                                      ))
                               ), U),
                       sort(U, L)
                     ),
                     [file(Source)|Options]),
    move_term(Term,
              Into,
              end_of_file,
              cond_move_pred(Term, MSource, FTarget, PredList, Into),
              [file(Source)], true, [file(Target)], Options),
    forall(move_predicates_hook(PredList, MSource, Source, MTarget, Target, Options), true),
    cleanup_use_module(MSource, PredList, [file(Target)|Options]),
    cleanup_declarations(MSource, MTarget, PredList, [file(Target)|Options]),
    add_exports_module(MSource, Target, PredList, Options),
    add_use_mod(target, MSource, Target, PredList, [file(Source)|Options]),
    add_use_mod(source, MSource, Source, PredList, [file(Target)|Options]),
    add_new_use_module(MSource, MTarget, Source, Target, PredList, Options),
    ( option(update_db(true), Options)
    ->update_db(PredList, MSource, MTarget, FTarget)
    ; true
    ).
