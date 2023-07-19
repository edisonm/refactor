/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.
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

:- module(ref_scenarios,
          [rename_variable/3,
           rename_variables/2,
           underscore_singletons/1,
           anonymize_singletons/1,
           anonymize_underscore_multi/1,
           anonymize_all_singletons/1,
           new_name/3,
           fix_multi_singletons/1,
           anonymize_term_singletons/2,
           replace_term_id/3,
           unfold_goal/2,
           rename_predicate/3,
           rename_functor/3,
           remove_useless_exports/1,
           remove_underscore_multi/1,
           replace_conjunction/3,
           replace_conjunction/4,
           call_to_predicate/3,
           reformat_sentence/2,
           reformat_sentence/3,
           move_term/4,
           move_term/8,
           remove_call/2,
           remove_call/3
          ]).

:- use_module(library(lists)).
:- use_module(library(occurs)).
:- use_module(library(ordsets)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(substitute)).
:- use_module(library(option_utils)).
:- use_module(library(ref_context)).
:- use_module(library(ref_message)).
:- use_module(library(ref_replace)).
:- use_module(library(ref_replacers)).
:- use_module(library(clambda)).
:- use_module(library(list_sequence)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(prolog_clause), []).

:- meta_predicate
    apply_var_renamer(2, :),
    rename_variable(?,+,:),
    remove_useless_exports(:),
    unfold_goal(0,+),
    underscore_singletons(:),
    anonymize_underscore_multi(:),
    remove_underscore_multi(:),
    anonymize_all_singletons(:),
    anonymize_term_singletons(+,:),
    anonymize_singletons(:),
    fix_multi_singletons(:),
    rename_variables(+,:),
    rename_functor(+,+,:),
    replace_term_id(+,+,:).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Most used refactoring scenarios:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_useless_exports(MO:Options1) :-
    select_option(module(M), Options1, Options, M),
    replace_sentence((:-module(M,L)), (:- module(M,N)),
                    ( include(being_used(M), L, N),
                      L \= N
                    ), MO:[module(M)|Options]),
    replace_sentence((:-export(K)), Exp,
                    ( once(list_sequence(L, K)),
                      include(being_used(M), L, N),
                      ( N = []                   -> Exp = []
                      ; L \= N, list_sequence(N,S), Exp = (:- export(S))
                      )
                    ), MO:[module(M)|Options]).

being_used(M, F/A) :-
    functor(H, F, A),
    predicate_property(C:H, imported_from(M)), C \== user.

apply_var_renamer(Renamer, MO:Options1) :-
    foldl(select_option_default,
          [variable_names(Dict)-Dict],
          Options1, Options),
    replace_term(Var, '$VAR'(Name),
                 ( var(Var),
                   member(Name1 = Var1, Dict),
                   Var1==Var
                 ->call(Renamer, Name1, Name),
                   \+ memberchk(Name=_, Dict)
                 ),
                 MO:[variable_names(Dict)|Options]).

/*
apply_var_renamer(Renamer, MO:Options1) :-
    foldl(select_option_default,
          [variable_names(Dict)-Dict],
          Options1, Options),
    replace_sentence(Sent1, Sent,
                     ( foldl(var_renamer_each(Renamer, Dict), Dict, Sent1, Sent),
                       Sent1 \== Sent
                     ),
                     MO:[variable_names(Dict)|Options]).

var_renamer_each(Renamer, Dict, Name1 = Var1) -->
    ( { call(Renamer, Name1, Name),
        \+ memberchk(Name = _, Dict)
      }
    ->substitute_value(Var1, '$VAR'(Name))
    ; []
    ).
*/

%!  rename_variable(?Name1:atom, +Name:atom, :Options) is det.
%
%   Rename a variable in a Term, provided that the name is new in such term.

rename_variable(Name1, Name, Options) :-
    apply_var_renamer([Name1, Name] +\ Name1^Name^true, Options).

underscore_singletons(MO:Options1) :-
    foldl(select_option_default,
          [sentence(Sent)-Sent,
           variable_names(Dict)-Dict],
          Options1, Options),
    apply_var_renamer([Dict, Sent] +\ Name1^Name
                     ^( member(Name1=Var, Dict),
                        \+ atom_concat('_', _, Name1),
                        occurrences_of_var(Var, Sent, 1),
                        atom_concat('_', Name1, Name)
                      ), MO:[sentence(Sent), variable_names(Dict)|Options]).

anonymize_underscore_multi(MO:Options1) :-
    foldl(select_option_default,
          [sentence(Sent)-Sent,
           variable_names(Dict)-Dict],
          Options1, Options),
    apply_var_renamer([Dict, Sent] +\ Name1^Name
                     ^( member(Name1=Var, Dict),
                        atom_concat('_', Name2, Name1),
                        atom_codes(Name2, [C|_]),
                        char_type(C, csymf),
                        occurrences_of_var(Var, Sent, N),
                        N > 1,
                        Name = '_'
                      ), MO:[sentence(Sent), variable_names(Dict)|Options]).

remove_underscore_multi(MO:Options1) :-
    foldl(select_option_default,
          [sentence(Sent)-Sent,
           variable_names(Dict)-Dict],
          Options1, Options),
    apply_var_renamer([Dict, Sent] +\ Name1^Name
                     ^( member(Name1=Var, Dict),
                        atom_concat('_', Name, Name1),
                        atom_codes(Name, [C|_]),
                        char_type(C, csymf),
                        occurrences_of_var(Var, Sent, N),
                        N > 1,
                        ( member(Name=_, Dict)
                        ->refactor_message("Cannot rename ~w to ~w since it already exists",
                                           [Name1, Name]),
                          fail
                        ; true
                        )
                      ), MO:[sentence(Sent), variable_names(Dict)|Options]).

anonymize_all_singletons(MO:Options1) :-
    foldl(select_option_default,
          [sentence(Sent)-Sent,
           variable_names(Dict)-Dict],
          Options1, Options),
    apply_var_renamer([Dict, Sent] +\ Name1^Name
                     ^( member(Name1=Var, Dict),
                        occurrences_of_var(Var, Sent, 1),
                        Name = '_'
                      ), MO:[sentence(Sent), variable_names(Dict)|Options]).

anonymize_term_singletons(Term, MO:Options1) :-
    foldl(select_option_default,
          [sentence(Sent)-Sent,
           variable_names(Dict)-Dict],
          Options1, Options),
    apply_var_renamer([Term, Dict, Sent] +\ Name1^Name
                     ^( member(Name1=Var, Dict),
                        occurrences_of_var(Var, Sent, 1),
                        \+ occurrences_of_var(Var, Term, 0 ),
                        Name = '_'
                      ), MO:[sentence(Sent), variable_names(Dict)|Options]).

anonymize_singletons(MO:Options1) :-
    foldl(select_option_default,
          [sentence(Sent)-Sent,
           variable_names(Dict)-Dict],
          Options1, Options),
    apply_var_renamer([Dict, Sent] +\ Name1^Name
                     ^( member(Name1=Var, Dict),
                        \+ atom_concat('_', _, Name1),
                        occurrences_of_var(Var, Sent, 1),
                        Name = '_'
                      ), MO:[sentence(Sent), variable_names(Dict)|Options]).

:- meta_predicate new_name(1, +, -).
new_name(AlreadyUsedName, Name, Name) :-
    \+ call(AlreadyUsedName, Name), !.
new_name(AlreadyUsedName, Name1, Name) :-
    new_name_rec(AlreadyUsedName, 2, Name1, Name).

new_name_rec(AlreadyUsedName, Idx, Name1, Name) :-
    atomic_concat(Name1, Idx, Name),
    \+ call(AlreadyUsedName, Name), !.
new_name_rec(AlreadyUsedName, Idx1, Name1, Name) :-
    succ(Idx1, Idx),
    new_name_rec(AlreadyUsedName, Idx, Name1, Name).

fix_multi_singletons(MO:Options1) :-
    foldl(select_option_default,
          [sentence(Sent)-Sent,
           variable_names(Dict)-Dict],
          Options1, Options),
    apply_var_renamer([Dict, Sent] +\ Name1^Name
                     ^( member(Name1=Var, Dict),
                        atom_concat('_', Name2, Name1),
                        occurrences_of_var(Var, Sent, N),
                        N > 1,
                        new_name([Dict]+\ X^member(X=_, Dict), Name2, Name)
                      ), MO:[sentence(Sent), variable_names(Dict)|Options]).

rename_variables(RenameL, Options) :-
    apply_var_renamer([RenameL] +\ Name1^Name^member(Name1=Name, RenameL),
                      Options).

rename_functor(Functor/Arity, NewName, Options) :-
    functor(Term, Functor, Arity),
    Term =.. [_|Args],
    Into =.. [NewName|Args],
    replace_term_id(Term, Into, Options).

replace_term_id(Term, Into, Options) :-
    replace_term(Term, Into, Options),
    functor(Term, F1, A1),
    functor(Into, F, A),
    replace_term(F1/A1, F/A, Options).

% BUG: This have to be applied only once --EMM
:- meta_predicate rename_predicate(+,+,:).
rename_predicate(M:Name1/Arity, Name, Options1) :-
    functor(H1, Name1, Arity),
    H1 =.. [Name1|Args],
    H  =.. [Name|Args],
    select_option(module(CM), Options1, Options2, CM),
    merge_options(Options2, [module(CM)], Options),
    replace_goal(H1, H,
                 ( predicate_property(CM:H1, imported_from(M))
                 ; M = CM
                 ),
                 Options),      % Replace calls
    % Replace heads:
    replace_head(H1, H, true, Options),
    replace_head((M:H1), (M:H), true, Options),
    replace_term(M:Name1/Arity, M:Name/Arity, Options),
    replace_term(Name1/Arity, Name/Arity,
                 ( catch(absolute_file_name(Alias, File, [file_type(prolog)]),
                         _, fail),
                   module_property(M, file(File))
                 ),
                 [sentence((:- use_module(Alias, _)))|Options1]),
    ( CM = M
    ->           % Replace PIs, but only inside the module, although this part
                 % is `complete but not correct'
      replace_term(Name1/Arity, Name/Arity, Options)
    ; true
    ).

:- dynamic add_import/4.

unfold_body_arg(IM, CM, Spec, Arg1, Arg) :-
    nonvar(Arg1),
    ( integer(Spec)
    ; Spec = (^)
    ), !,
    strip_module(IM:Arg1, NM, Arg2),
    unfold_body(Arg2, Arg, NM, CM).
unfold_body_arg(_, _, _, Arg, Arg).

:- use_module(library(mapargs)).
:- init_expansors.

unfold_body(M:Body1, Body, _, CM) :- !, unfold_body(Body1, Body, M, CM).
unfold_body(Body1, Body, IM, CM) :-
    ( CM == IM
    ->Body = Body1
    ; predicate_property(IM:Body1, implementation_module(IM1)),
      ( predicate_property(CM:Body1, defined)
      ->predicate_property(CM:Body1, implementation_module(IM2))
      ; predicate_property(IM:Body1, exported)
      ->IM2 = IM1,
        functor(Body1, F, A),
        assertz(add_import(CM, IM1, F, A))
      ; IM2 = CM %% Make IM2 different than IM1
      ),
      ( predicate_property(IM:Body1, meta_predicate(Meta)),
        arg(_, Meta, Spec),
        ( integer(Spec)
        ; Spec = (^)
        )
      ->functor(Body1, F, A),
        functor(Body2, F, A),
        mapargs(unfold_body_arg(IM, CM), Meta, Body1, Body2)
      ; Body2 = Body1
      ),
      ( IM1 \= IM2
      ->Body = IM:Body2
      ; Body = Body2
      )
    ).

:- use_module(library(infer_alias)).

rsum(Module, UML) :-
    findall(Import-(F/A), retract(add_import(Module, Import, F, A)), Pairs),
    Pairs \= [],
    sort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    findall(UM,
            ( member(Import-IL, Grouped),
              module_property(Import, file(IFile)),
              smallest_alias(IFile, IA),
              UM = '$@'(:- use_module(IA, '$C'((nl,write('\t     ')),'$LIST,NL'(IL))))
            ), UML).

is_member(VarL, E) :-
    member(V, VarL),
    V == E, !.

set_new_name(VNBody, VN, V) :-
    ( member(Name1=V1, VNBody),
      V1 == V
    ->( ( Name = Name1
        ; between(2, inf, Count),
          atomic_concat(Name1, Count, Name)
        ),
        \+ member(Name=_, VN)
      ->V = '$VAR'(Name)
      )
    ; true % Leave unnamed
    ).

match_clause_head_body((Head :- Body),   _:Head, Body) :- !.
match_clause_head_body((M:Head :- Body), M:Head, Body) :- !.
match_clause_head_body(Head, Head, true).

:- meta_predicate qualify_meta_call(0, ?, -).

qualify_meta_call(M:Goal1, CM, M:Goal) :-
    qualify_meta_call(Goal1, M, CM, true, Goal).

% NOTE: Only works if exactly one clause match
unfold_goal(MGoal, MO:Options1) :-
    MGoal = M:Goal,
    strip_module(MGoal, M, Goal),
    select_option(module(Module), Options1, Options, Module),
    qualify_meta_call(MGoal, Module, MMeta),
    retractall(add_import(_, _, _, _)),
    replace_goal(Goal, '$BODY'(Body),
                 ( findall(clause(MMeta, Body1, CM, VNBody),
                           ( clause(MMeta, _, Ref),
                             clause_property(Ref, line_count(Line)),
                             clause_property(Ref, file(File)),
                             clause_property(Ref, module(CM)),
                             prolog_clause:read_term_at_line(File, Line, CM, Clause, _, VNBody),
                             match_clause_head_body(Clause, MMeta, Body1) % Raw Body before expansion
                           ), [clause(MMeta, Body1, CM, VNBody)]),
                   unfold_body(Body1, Body, CM, Module),
                   term_variables(Body, VarL),
                   term_variables(VN, VarS),
                   exclude(is_member(VarS), VarL, NewVarL),
                   maplist(set_new_name(VNBody, VN), NewVarL)
                 ),
                 MO:[module(Module), variable_names(VN)|Options]),
    replace_sentence((:- use_module(Alias, L1)), [(:- use_module(Alias, '$LISTB,NL'(L)))],
                     ( catch(absolute_file_name(Alias, IFile, [file_type(prolog)]),
                             _,
                             fail),
                       module_property(Import, file(IFile)),
                       findall(F/A, retract(add_import(Module, Import, F, A)), UL),
                       UL \= [],
                       sort(UL, L2),
                       append(L1, L2, L)
                     ),
                     MO:[module(Module)|Options]),
    replace_sentence((:- module(Module, L)), [(:- module(Module, L))|UML],
                     rsum(Module, UML),
                     MO:[module(Module)|Options]).

%%  remove_call(Call, :Expander, :Options) is det
%
%   Remove occurrences of Call in the matching term. Note that to be fully
%   effective should be used in conjunction with the option fixpoint(file).

:- meta_predicate remove_call(+,0,:).
remove_call(Call, Expander, Options) :-
    replace_term(Term, Into,
                 ( refactor_context(pattern, P),
                   do_remove_call(Call, Term, P, X),
                   Expander,
                   refactor_context(into, X),
                   copy_term(P-X, Term-Into)
                 ), Options).

:- meta_predicate remove_call(+,:).
remove_call(Call, Options) :-
    remove_call(Call, true, Options).

do_remove_call(Call, Term, P, X) :-
    ( subsumes_term((_ :- Call), Term),
      P = (X :- _),
      Term = (_ :- Call)
    ; subsumes_term((Call, _), Term),
      P = (_, X),
      Term = (Call, _)
    ; subsumes_term((_, Call), Term),
      P = (X, _),
      Term = (_, Call)
    ; subsumes_term(Call, Term),
      refactor_context(sentence, Sent),
      Term \== Sent,
      X = true,
      Term = Call
    ).

:- meta_predicate replace_conjunction(?, ?, 0, :).
replace_conjunction(Conj, Repl, Expander, MO:Options1) :-
    replace_last_literal_(Conj, Conj2, CLit, CBody),
    replace_last_literal(Repl, Repl1, RLit, RBody),
    add_body_hook_if_needed(Conj, Repl1, Repl2),
    copy_term(t(Conj2, CLit, CBody, Repl2, RLit, RBody), Term),
    copy_term(Conj, ConjP),
    foldl(select_option_default,
          [decrease_metric(Metric)-(ref_scenarios:conj_pattern_size(ConjP))],
          Options1, Options),
    replace(body_rec, Conj2, Repl2,
            ( bind_lit_body(Term, Conj2, CLit, CBody, RLit, RBody),
              Expander
            ), MO:[decrease_metric(Metric)|Options]).

:- public conj_pattern_size/4.
conj_pattern_size(Conj, Term, _, Size) :-
    ref_replace:pattern_size(Term, Conj, Size).

add_body_hook_if_needed(Conj, Repl1, Repl) :-
    ( var(Conj)
    ; Conj \= (_, _)
    ), !,
    ( ( var(Repl1)
      ; Repl1 \= (_, _)
      )
    ->Repl = Repl1
    ; Repl = '$BODYB'(Repl1)
    ).
add_body_hook_if_needed((_, Conj), Repl1, Repl) :-
    ( ( var(Repl1)
      ; Repl1 \= (_, _)
      )
    ->Repl = Repl1
    ; Repl1 = (RLit, Repl2),
      Repl = (RLit, Repl3),
      add_body_hook_if_needed(Conj, Repl2, Repl3)
    ).

:- meta_predicate replace_conjunction(?, ?, :).
replace_conjunction(Conj, Replacement, Options) :-
    replace_conjunction(Conj, Replacement, true, Options).

replace_last_literal(Conj, Body, Conj, Body) :- var(Conj), !.
replace_last_literal((A, Conj), (A, Conj2), Lit, Body) :- !,
    replace_last_literal_(Conj, Conj2, Lit, Body).
replace_last_literal(Conj, Body, Conj, Body).

replace_last_literal_(Conj, Body, Conj, Body) :- var(Conj), !.
replace_last_literal_((A,Conj), (A,Conj2), Lit, Body) :- !,
    replace_last_literal_(Conj, Conj2, Lit, Body).
replace_last_literal_(Conj, Body, Conj, Body).

bind_lit_body(Term, Conj2, CLit, CBody, RLit, RBody) :-
    ( subsumes_term(Conj2-CLit, Conj2-CBody)
    ->CBody = CLit,
      RBody = RLit,
      PCBody = PCLit,
      PRBody = PRLit
    ; subsumes_term(Conj2-(CLit, Rest), Conj2-CBody)
    ->CBody = (CLit, Rest),
      RBody = (RLit, Rest) $@ CBody,
      PCBody = (PCLit, PRest),
      PRBody = (PRLit, PRest) $@ PCBody
    ),
    Term = t(Conj, PCLit, PCBody, Repl, PRLit, PRBody),
    refactor_context(pattern, Conj),
    refactor_context(into,    Repl).

:- dynamic
       new_pred/3.

call_to_predicate(Term, Suffix, OptL) :-
    replace(body_rec, Term,
            '$LIST'([Pred,
                     '$NOOP'('$G'('$PRIORITY'('$CLAUSE'(Pred :- Term), 1200),
                                  ref_scenarios:ctp_1(F, Sent)))]),
            ( substitute_value(Term, -, Sent, STrm),
              term_variables(STrm, SVarU),
              term_variables(Term, TVarU),
              sort(SVarU, SVarL),
              sort(TVarU, TVarL),
              ord_intersect(SVarL, TVarL, ArgL),
              Sent = (Head :- _),
              functor(Head, Prefix, _),
              atomic_list_concat([Prefix, '_', Suffix], Name),
              Pred =.. [Name|ArgL]
            ), [file(F), fixpoint(none), sentence(Sent)|OptL]),
    findall(File, new_pred(File, _, _), FileU),
    sort(FileU, FileL),
    replace_sentence(Sent, ['$TEXT'(S), Sent],
                     retract(new_pred(File, Sent, S)),
                     [file(F), files(FileL), sentence(Sent)|OptL]).

:- public
       ctp_1/4.

ctp_1(F, (H:-_), S, _) :- assertz(new_pred(F, (H:-_), S)), fail.

sentence_format(Clause, '$CLAUSE'(Clause)).

:- meta_predicate reformat_sentence(?, 0, :).
reformat_sentence(Term, Expander, Options) :-
    replace_sentence(Term, Formatted,
                     ( duplicate_term(Term, Copy),
                       sentence_format(Copy, Formatted),
                       Copy = Term,
                       Expander
                     ),
                     Options).

:- meta_predicate reformat_sentence(?, :).
reformat_sentence(Term, Options) :-
    reformat_sentence(Term, true, Options).

:- dynamic
        subtext_db/1.

:- public record_text/0.

record_text :-
    refactor_context(text, Text),
    refactor_context(subpos, SubPos),
    arg(1, SubPos, From),
    arg(2, SubPos, To),
    ref_replace:get_subtext(Text, From, To, SubText),
    assertz(subtext_db(SubText)).

:- meta_predicate move_term(?,?,+,0,+,0,+,+).

move_term(Term, SourceOpts, TargetOpts, CommonOpts) :-
    move_term(Term, [], end_of_file, true, SourceOpts, true, TargetOpts, CommonOpts).

move_term(Term, Into, AddAt, SourceCond, SourceOpts, TargetCond, TargetOpts, CommonOpts) :-
    merge_options(SourceOpts, CommonOpts, SOptions),
    merge_options([fixpoint(true)|TargetOpts], CommonOpts, TOptions),
    replace_sentence(Term,
                     '$C'(ref_scenarios:record_text, Into),
                     SourceCond,
                     SOptions),
    replace_sentence(AddAt, '$TEXT'(SubText),
                     ( retract(subtext_db(SubText)),
                       TargetCond
                     ),
                     TOptions).
