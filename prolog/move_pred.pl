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
          [ move_predicates/4
          ]).

:- use_module(library(assertions)).
:- use_module(library(list_sequence)).
:- use_module(library(module_links)).
:- use_module(library(sequence_list)).
:- use_module(library(pretty_decl)).

:- multifile
        cond_move_pred_hook/4,
        move_predicates_hook/6.

depends_of(AH, AM, H, M, CM, N) :-
    depends_of_db(AH, AM, H, M, CM, N).

cond_move_pred(Term, _, _, _) :-
    var(Term),
    !,
    fail.
cond_move_pred(Term, MSource, PredList, Into) :-
    cond_move_pred_hook(Term, MSource, PredList, Into),
    !.
cond_move_pred(Term, M, PredList, []) :-
    memberchk(Term,
              [ G --> _
              ]),
    !,
    functor(G, F, D),
    A is D + 2,
    memberchk(M:F/A, PredList).
cond_move_pred(Term, M, PredList, []) :-
    memberchk(Term,
              [ (H :- _)
              ]),
    !,
    nonvar(H),
    functor(H, F, A),
    memberchk(M:F/A, PredList).
cond_move_pred((:- module(_, _)), _, _, _) :-
    !,
    fail.
cond_move_pred((:- include(Alias)), _, _, (:- include(Alias))) :-
    % We asume you don't use include to add predicates... do you???
    !.
cond_move_pred((:- Decl), MSource, PredList, Into) :-
    Decl =.. [F, Sequence],
    memberchk(F, [(meta_predicate), (multifile), (discontiguous), (dynamic),
                  (thread_local), (public), (export)]),
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
      Into =.. [F, ISeq]
    ).
cond_move_pred((:- use_module(Alias)), MSource, PredList, Into) :-
    !,
    absolute_file_name(Alias, File, [file_errors(fail), file_type(prolog)]),
    module_property(M, file(File)),
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
cond_move_pred((:- use_module(Alias, ExL1)), MSource, PredList, Into) :-
    !,
    absolute_file_name(Alias, File, [file_errors(fail), file_type(prolog)]),
    module_property(M, file(File)),
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
cond_move_pred(H, M, PredList, []) :-  % This must be the last clause
    functor(H, F, A),
    memberchk(M:F/A, PredList).

add_exports_module(MSource, Target, PredList, Options) :-
    pretty_decl((:- module(MTarget, L)), Decl),
    replace_sentence((:- module(MTarget, L1)), Decl,
                     ( findall(F/A,
                               ( member(F/A, PredList),
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
                     ( absolute_file_name(Alias, File, [file_type(prolog)]),
                       module_property(M, file(File)),
                       findall(F/A,
                               ( member(F/A, ExL1),
                                 functor(H, F, A),
                                 once(( depends_of(H2, M2, H, M, MSource, 1),
                                        functor(H2, F2, A2),
                                        memberchk(M2:F2/A2, PredList)
                                      ))
                               ), ExL)
                     ), Options).

:- dynamic
    declared_db/3.

add_new_use_module(MSource, MTarget, Source, Target, PredList, Options) :-
    findall(CM,
            ( depends_of_db(_, _, H, MSource, CM, 1),
              CM \= MSource,
              functor(H, F, A),
              memberchk(MSource:F/A, PredList),
              \+ ( depends_of_db(_, _, H, MSource, CM, 1),
                   CM \= MSource,
                   functor(H, F, A),
                   \+ memberchk(MSource:F/A, PredList)
                 )
            ), CMU),
    sort(CMU, CML),
    replace_sentence((:- use_module(Source)), Into,
                     ( phrase(( ( { depends_of_db(_, _, H, MSource, CM, 1),
                                      functor(H, F, A),
                                      \+ memberchk(MSource:F/A, PredList)
                                    }
                                  ->[(:- use_module(Source))]
                                  ; []
                                  ),
                                  ( {CM \= MTarget}
                                  ->[(:- use_module(Target))]
                                  ; []
                                  )
                                ), Into)
                     ), [module(CM), modules(CML)|Options]),
    replace_sentence((:- Decl), [],
                     ( memberchk(Decl, [include(_), use_module(_), use_module(_, _)]),
                       ( declared_db(CM, File, Decl)
                       ->true
                       ; assertz(declared_db(CM, File, Decl)),
                         fail
                       )
                     ), [file(File), module(CM), modules([MTarget|CML])|Options]),
    retractall(declared_db(_, _, _)).

add_target_use_mod(MSource, Source, Target, PredList, Options1) :-
    ( depends_of(H2, M2, H, MSource, MSource, 1),
      functor(H, F, A),
      memberchk(MSource:F/A, PredList),
      functor(H2, F2, A2),
      \+ memberchk(M2:F2/A2, PredList)
    ->Options = [file(Source), max_changes(1), changes(C)|Options1],
      once(( ( replace_sentence([(:- use_module(Alias)), Next],
                                [(:- use_module(Alias)), (:- use_module(Target)), Next],
                                Next \= (:- use_module(_)),
                                Options)
             ; member(Term, [(:- include(_)), (:- module(_, _))]),
               replace_sentence(Term, [Term, (:- use_module(Target))], Options)
             ),
             C \= 0
           ))
    ; true
    ).

/*
    forall(member(CM, CML),
           cleanup_duplicated_use_module(CM, Target, Options)).

cleanup_duplicated_use_module(CM, Target, Options) :-
    replace_sentence((:- use_module(Target)), [],
                     ( refactor_context(tries, Tries),
                       Tries > 1
                     ), [module(CM)|Options]).
*/

mark_exclude(M, PredList, PI1, PI) :-
    ( PI1 = F/A,
      memberchk(M:F/A, PredList)
    ->PI = '$RM'
    ; PI = PI1
    ).

normalize_pred_id(M1, PI, M:F/A) :-
    ( PI = M:F/A
    ->true
    ; PI = F/A,
      M = M1
    ).

move_predicates(PredList1, Source, Target, Options) :-
    absolute_file_name(Source, FSource, [file_type(prolog)]),
    module_property(MSource, file(FSource)), % This should exist
    absolute_file_name(Target, FTarget, [file_type(prolog)]),
    maplist(normalize_pred_id(MSource), PredList1, PredList),
    ( module_property(MTarget, file(FTarget))
    ->true
    ; file_name_extension(BaseDir, _, FTarget),
      directory_file_path(_, MTarget, BaseDir)
    ),
    ( exists_file(FTarget)
    ->true
    ; tell(FTarget),
      portray_clause((:- module(MTarget, []))),
      told
    ),
    replace_sentence((:- module(M, L1)), (:- module(M, L)),
                     ( maplist(mark_exclude(MSource, PredList), L1, L),
                       L \= L1
                     ),
                     [file(Source)|Options]),
    move_term(Term,
              Into,
              end_of_file,
              cond_move_pred(Term, MSource, PredList, Into),
              [file(Source)], true, [file(Target)], Options),
    forall(move_predicates_hook(PredList, MSource, Source, MTarget, Target, Options), true),
    cleanup_use_module(MSource, PredList, [file(Target)|Options]),
    add_exports_module(MSource, Target, PredList, Options),
    add_new_use_module(MSource, MTarget, Source, Target, PredList, Options),
    add_target_use_mod(MSource, Source, Target, PredList, Options).
