/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.

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

:- module(ref_scenarios,
	  [replace_term/3,
	   replace_head/3,
	   replace_body/3,
	   replace_goal/3,
	   replace_sentence/3,
	   rename_variable/3,
	   rename_variables/2,
	   underscore_singletons/1,
	   anonymize_singletons/1,
	   anonymize_all_singletons/1,
	   new_name/3,
	   fix_multi_singletons/1,
	   anonymize_term_singletons/2,
	   replace_term_id/3,
	   unfold_goal/2,
	   rename_predicate/3,
	   rename_functor/3,
	   remove_useless_exports/1,
	   replace_conjunction/3,
	   replace_conjunction/4,
	   remove_call/2,
	   remove_call/3
	  ]).

:- use_module(library(implementation_module)).
:- use_module(library(option_utils)).
:- use_module(library(ref_context)).
:- use_module(library(ref_replace)).
:- use_module(library(ref_replacers)).
:- use_module(library(clambda)).
:- use_module(library(list_sequence)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(prolog_clause), []).

:- meta_predicate
	unfold_goal(0,+).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Most used refactoring scenarios:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_useless_exports(Options0) :-
    select_option(module(M), Options0, Options, M),
    replace_sentence((:-module(M,L)), (:- module(M,N)),
		    ( include(being_used(M), L, N),
		      L \= N
		    ), [module(M)|Options]),
    replace_sentence((:-export(K)), Exp,
		    ( once(list_sequence(L, K)),
		      include(being_used(M), L, N),
		      ( N = []                   -> Exp = []
		      ; L \= N, list_sequence(N,S), Exp = (:- export(S))
		      )
		    ), [module(M)|Options]).

being_used(M, F/A) :-
    functor(H, F, A),
    predicate_property(C:H, imported_from(M)), C \== user.

:- meta_predicate apply_var_renamer(2, +).
apply_var_renamer(Renamer, OptionL0 ) :-
    foldl(select_option_default,
	  [sentence(Sent)-Sent,
	   variable_names(Dict)-Dict],
	  OptionL0, OptionL),
    replace_sentence(Sent, Sent,
		     ( findall(Name0='$VAR'(Name),
			       ( call(Renamer, Name0, Name),
				 \+ memberchk(Name=_, Dict)
			       ), ARenameL),
		       intersection(ARenameL, Dict, AppliedL),
		       AppliedL \= []
		     ), [sentence(Sent), variable_names(Dict)|OptionL]).

%% rename_variable(?Name0:atom, +Name:atom, +Options) is det.
%
% Rename a variable in a Term, provided that the name is new in such term.

rename_variable(Name0, Name, Options) :-
    apply_var_renamer([Name0, Name] +\ Name0^Name^true, Options).

underscore_singletons(OptionL0 ) :-
    foldl(select_option_default,
	  [sentence(Sent)-Sent,
	   variable_names(Dict)-Dict],
	  OptionL0, OptionL),
    apply_var_renamer([Dict, Sent] +\ Name0^Name
		     ^( member(Name0=Var, Dict),
			\+ atom_concat('_', _, Name0),
			occurrences_of_var(Var, Sent, 1),
			atom_concat('_', Name0, Name)
		      ), [sentence(Sent), variable_names(Dict)|OptionL]).

anonymize_all_singletons(OptionL0 ) :-
    foldl(select_option_default,
	  [sentence(Sent)-Sent,
	   variable_names(Dict)-Dict],
	  OptionL0, OptionL),
    apply_var_renamer([Dict, Sent] +\ Name0^Name
		     ^( member(Name0=Var, Dict),
			occurrences_of_var(Var, Sent, 1),
			Name = '_'
		      ), [sentence(Sent), variable_names(Dict)|OptionL]).

anonymize_term_singletons(Term, OptionL0 ) :-
    foldl(select_option_default,
	  [sentence(Sent)-Sent,
	   variable_names(Dict)-Dict],
	  OptionL0, OptionL),
    apply_var_renamer([Term, Dict, Sent] +\ Name0^Name
		     ^( member(Name0=Var, Dict),
			occurrences_of_var(Var, Sent, 1),
			\+ occurrences_of_var(Var, Term, 0 ),
			Name = '_'
		      ), [sentence(Sent), variable_names(Dict)|OptionL]).


anonymize_singletons(OptionL0 ) :-
    foldl(select_option_default,
	  [sentence(Sent)-Sent,
	   variable_names(Dict)-Dict],
	  OptionL0, OptionL),
    apply_var_renamer([Dict, Sent] +\ Name0^Name
		     ^( member(Name0=Var, Dict),
			\+ atom_concat('_', _, Name0 ),
			occurrences_of_var(Var, Sent, 1),
			Name = '_'
		      ), [sentence(Sent), variable_names(Dict)|OptionL]).

:- meta_predicate new_name(1, +, -).
new_name(AlreadyUsedName, Name, Name) :-
    \+ call(AlreadyUsedName, Name), !.
new_name(AlreadyUsedName, Name0, Name) :-
    new_name_rec(AlreadyUsedName, 2, Name0, Name).

new_name_rec(AlreadyUsedName, Idx, Name0, Name) :-
    atomic_concat(Name0, Idx, Name),
    \+ call(AlreadyUsedName, Name), !.
new_name_rec(AlreadyUsedName, Idx0, Name0, Name) :-
    succ(Idx0, Idx),
    new_name_rec(AlreadyUsedName, Idx, Name0, Name).

fix_multi_singletons(OptionL0 ) :-
    foldl(select_option_default,
	  [sentence(Sent)-Sent,
	   variable_names(Dict)-Dict],
	  OptionL0, OptionL),
    apply_var_renamer([Dict, Sent] +\ Name0^Name
		     ^( member(Name0=Var, Dict),
			atom_concat('_', Name1, Name0 ),
			occurrences_of_var(Var, Sent, N),
			N > 1,
			new_name([Dict]+\ X^member(X=_, Dict), Name1, Name)
		      ), [sentence(Sent), variable_names(Dict)|OptionL]).

rename_variables(RenameL, Options) :-
    apply_var_renamer([RenameL] +\ Name0^Name^member(Name0=Name, RenameL),
		      Options).

rename_functor(Functor/Arity, NewName, Options) :-
    functor(Term, Functor, Arity),
    Term =.. [_|Args],
    Expansion =.. [NewName|Args],
    replace_term_id(Term, Expansion, Options).

replace_term_id(Term, Expansion, Options) :-
    replace_term(Term, Expansion, Options),
    functor(Term, F0, A0),
    functor(Expansion, F, A),
    replace_term(F0/A0, F/A, Options).

% BUG: This have to be applied only once --EMM
:- meta_predicate rename_predicate(+,+,+).
rename_predicate(M:Name0/Arity, Name, OptionL0) :-
    functor(H0, Name0, Arity),
    H0 =.. [Name0|Args],
    H  =.. [Name|Args],
    select_option(module(CM), OptionL0, OptionL1, CM),
    OptionL = [module(CM)|OptionL1],
    replace_goal(H0, H,
		 ( predicate_property(CM:H0, imported_from(M))
		 ; M = CM
		 ),
		 OptionL),	% Replace calls
    % Replace heads:
    replace_head(H0, H, true, OptionL),
    replace_head((M:H0), (M:H), true, OptionL),
    replace_term(M:Name0/Arity, M:Name/Arity, OptionL),
    replace_term(Name0/Arity, Name/Arity,
		 ( catch(absolute_file_name(Alias, File, [file_type(prolog)]),
			 _, fail),
		   current_module(M, File)
		 ),
		 [sentence((:- use_module(Alias, _)))|OptionL0]),
    ( CM = M
    ->		 % Replace PIs, but only inside the module, although this part
		 % is `complete but not correct'
      replace_term(Name0/Arity, Name/Arity, OptionL)
    ; true
    ).

replace_term(Term, Into, Options) :-
    replace_term(Term, Into, true, Options).

replace_head(Term, Into, Options) :-
    replace_head(Term, Into, true, Options).

replace_body(Term, Into, Options) :-
    replace_body(Term, Into, true, Options).

replace_sentence(Sentence, Into, Options) :-
    replace_sentence(Sentence, Into, true, Options).

:- meta_predicate replace_goal(?,?,+).
replace_goal(Term, Into, Options) :-
    replace_goal(Term, Into, true, Options).

:- dynamic add_import/4.

unfold_body_arg(IM, CM, _, Spec, Arg0, Arg) :-
    nonvar(Arg0),
    ( integer(Spec)
    ; Spec = (^)
    ), !,
    strip_module(IM:Arg0, NM, Arg1),
    unfold_body(Arg1, Arg, NM, CM).
unfold_body_arg(_, _, _, _, Arg, Arg).

:- use_module(library(mapargs)).

unfold_body(M:Body0, Body, _, CM) :- !, unfold_body(Body0, Body, M, CM).
unfold_body(Body0, Body, IM, CM) :-
    ( CM == IM
    ->Body = Body0
    ; implementation_module(IM:Body0, IM1),
      ( predicate_property(CM:Body0, defined)
      ->implementation_module(CM:Body0, IM2)
      ; predicate_property(IM:Body0, exported)
      ->IM2 = IM1,
	functor(Body0, F, A),
        assertz(add_import(CM, IM1, F, A))
      ; IM2 = CM %% Make IM2 different than IM1
      ),
      ( predicate_property(IM:Body0, meta_predicate(Meta)),
	arg(_, Meta, Spec),
	( integer(Spec)
	; Spec = (^)
	)
      ->functor(Body0, F, A),
	functor(Body1, F, A),
	mapargs(unfold_body_arg(IM, CM), Meta, Body0, Body1)
      ; Body1 = Body0
      ),
      ( IM1 \= IM2
      ->Body = IM:Body1
      ; Body = Body1
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
	      current_module(Import, IFile),
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
	; between(2, infinite, Count),
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

% NOTE: Only works if exactly one clause match
unfold_goal(MGoal, OptionL0) :-
    MGoal = M:Goal,
    select_option(module(Module), OptionL0, OptionL, Module),
    qualify_meta_goal(Goal, M, Module, Meta),
    MMeta = M:Meta,
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
		 [module(Module), variable_names(VN)|OptionL]),
    replace_sentence((:- use_module(Alias, L0)), [(:- use_module(Alias, '$LISTB,NL'(L)))],
		     ( catch(absolute_file_name(Alias, IFile, [file_type(prolog)]),
			     _,
			     fail),
		       current_module(Import, IFile),
		       findall(F/A, retract(add_import(Module, Import, F, A)), UL),
		       UL \= [],
		       sort(UL, L1),
		       append(L0, L1, L)
		     ),
		     [module(Module)|OptionL]),
    replace_sentence((:- module(Module, L)), [(:- module(Module, L))|UML],
		     rsum(Module, UML),
		     [module(Module)|OptionL]).

:- meta_predicate remove_call(+,0,+).
remove_call(Call, Expander, Options) :-
    replace(body_rec, Term, _, (do_remove_call(Term, Call), Expander),
	    Options).

remove_call(Call, Options) :-
    remove_call(Call, true, Options).

do_remove_call(Term, Call) :-
    ( subsumes_term((_ :- Call), Term)
    ->refactor_context(pattern, (X :- _)),
      Term = (_ :- Call)
    ; subsumes_term((Call, _), Term)
    ->refactor_context(pattern, (_, X)),
      Term = (Call, _)
    ; subsumes_term((_, Call), Term)
    ->refactor_context(pattern, (X, _)),
      Term = (_, Call)
    ; subsumes_term(Call, Term)
    ->X = true,
      Term = Call
    ),
    refactor_context(into, X).

:- meta_predicate replace_conjunction(?, ?, 0, +).
replace_conjunction(Conj, Repl, Expander, Options) :-
    replace_last_literal_(Conj, Conj2, CLit, CBody),
    replace_last_literal(Repl, Repl1, RLit, RBody),
    add_body_hook_if_needed(Conj, Repl1, Repl2),
    copy_term(t(Conj2, CLit, CBody, Repl2, RLit, RBody), Term),
    replace(body_rec, Conj2, Repl2,
	    ( bind_lit_body(Term, Conj2, CLit, CBody, RLit, RBody),
	      Expander
	    ), Options).

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
