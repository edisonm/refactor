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

:- module(move_assr, []).

:- use_module(library(substitute)).
:- use_module(library(assertions)).
:- use_module(library(move_preds)).

move_preds:cond_move_pred_hook((:- Assertions1), CM, PredList, Into) :-
    current_decomposed_assertion_1(Assertions1, _, CM, _, _, Body1, _, _, _, _, _, _),
    !,
    findall(M:F/A,
            ( decompose_assertion_head_body(Body1, _, CM, M:H, true, _, _, _, _, _, _, _, _),
              functor(H, F, A)
            ), AllPredList),
    ( subtract(AllPredList, PredList, [])
    ->Into = []
    ; substitute(assertions_exclude(AllPredList, PredList, CM), Body1, Body),
      substitute_value(Body1, Body, Assertions1, Assertions),
      Assertions \= Assertions1,
      Into = (:- Assertions)
    ).

move_preds:move_preds_hook(PredList, MSource, _, MTarget, Target, Options) :-
    cleanup_assertions(MSource, MTarget, PredList, [file(Target)|Options]).

cleanup_assertion(MSource, MTarget, PredList, Assertions1, AssertionsDecl) :-
    ( current_decomposed_assertion_1(Assertions1, _, MSource, _, _, Body1, _, _, _, _, _, _)
    ->findall(M:F/A,
              ( decompose_assertion_head_body(Body1, _, MSource, M:H, true, _, _, _, _, _, _, _, _),
                predicate_property(M:H, defined),
                predicate_property(M:H, implementation_module(MSource)),
                functor(H, F, A)
              ), AllPredList),
      AllPredList \= [],
      findall(M:F/A,
              ( decompose_assertion_head_body(Body1, _, MTarget, M:H, true, _, _, _, _, _, _, _, _),
                predicate_property(M:H, defined),
                functor(H, F, A)
              ), IncludePredList, PredList),
      ( intersection(AllPredList, IncludePredList, [])
      ->AssertionsDecl = []
      ; substitute(assertions_include(AllPredList, IncludePredList, MSource), Body1, Body),
        substitute_value(Body1, Body, Assertions1, Assertions),
        Assertions \= Assertions1,
        AssertionsDecl = (:- Assertions)
      )
    ).

cleanup_assertions(MSource, MTarget, PredList, Options) :-
    replace_sentence((:- Assertions1), AssertionsDecl,
                     cleanup_assertion(MSource, MTarget, PredList, Assertions1, AssertionsDecl),
                     Options).

assertions_include(AllPreds, PredList, M, Assertions1, Replace) :-
    assertions_include_exclude(AllPreds, PredList, M, Assertions1, Assertions1, '$RM', Replace).

assertions_exclude(AllPreds, PredList, M, Assertions1, Replace) :-
    assertions_include_exclude(AllPreds, PredList, M, Assertions1, '$RM', Assertions1, Replace).

assertions_include_exclude(AllPreds, PredList, CM, Body1, ReplaceInclude, ReplaceExclude, Replace) :-
    catch(
        ( \+ decompose_assertion_head_body(Body1, _, CM, _, _, _, _, _, _, _, _, _, _)
        ->fail
        ; forall(( decompose_assertion_head_body(Body1, _, CM, M:H, _, _, _, _, _, _, _, _, _),
                   functor(H, F, A)
                 ),
                 memberchk(M:F/A, AllPreds)),
          ( forall(( decompose_assertion_head_body(Body1, _, CM, M:H, _, _, _, _, _, _, _, _, _),
                     functor(H, F, A)
                   ),
                   \+ memberchk(M:F/A, PredList))
          ->Replace = ReplaceExclude
          ; forall(( decompose_assertion_head_body(Body1, _, CM, M:H, _, _, _, _, _, _, _, _, _),
                     functor(H, F, A)
                   ),
                   memberchk(M:F/A, PredList))
          ->Replace = ReplaceInclude
                      % ; fail
          )
        ),
        _,
        fail).
