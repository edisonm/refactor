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

:- use_module(library(ref_context)).
:- use_module(library(ref_replacers)).
:- use_module(library(list_sequence)).

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

%% rename_variable(?Name0:atom, +Name:atom, +Options) is det.
%
% Rename a variable in a Term, provided that such variable doesn't exist in such
% term.
rename_variable(Name0, Name, Options) :-
    replace_sentence(Sent, Sent,
		    ( \+ memberchk(Name=_, Dict),
		      memberchk(Name0='$VAR'(Name), Dict)
		    ), [variable_names(Dict)|Options]).

rename_variables(RenameL, Options) :-
    replace_sentence(Sent, Sent,
		    ( findall(Name0='$VAR'(Name),
			      ( ARename=(Name0=Name),
				member(ARename, RenameL),
				\+ memberchk(Name=_, Dict)
			      ), ARenameL),
		      intersection(ARenameL, Dict, AppliedL),
		      AppliedL \= []
		    ), [variable_names(Dict)|Options]).

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

rgbmm(Body0, Body, M, Module) :-
    ( Module == M
    ->Body = Body0
    ; predicate_property(Module:Body0, imported_from(_))
    ->( \+ ( predicate_property(Module:Body0, meta_predicate(Spec)),
	     arg(_, Spec, ASpec),
	     ( integer(ASpec)
	     ; ASpec = (^)
	     )
	   )
      ->Body = Body0
      ; Body = M:Body0
      )
    ; predicate_property(M:Body0, imported_from(M2))
    ->( M2 == Module
      ->Body = Body0
      ; ( \+ ( predicate_property(M:Body0, meta_predicate(Spec)),
	       arg(_, Spec, ASpec),
	       ( integer(ASpec)
	       ; ASpec = (^)
	       )
	     )
	->functor(Body0, F, A),
	  assertz(add_import(Module, M2, F, A)),
	  Body = Body0
	; Body = M:Body0
	)
      )
    ; Body = M:Body0
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

% NOTE: Only works if exactly one clause match
unfold_goal(MGoal, OptionL0) :-
    findall(clause(MGoal, Body0), clause(MGoal, Body0), [clause(MGoal, Body0)]),
    MGoal = M:Goal,
    select_option(module(Module), OptionL0, OptionL, Module),
    retractall(add_import(_, _, _, _)),
    replace_goal(Goal, Body,
		 rgbmm(Body0, Body, M, Module),
		 [module(Module)|OptionL]),
    replace_sentence((:- use_module(Alias, L0)), [(:- use_module(Alias, '$LIST,NL'(L)))],
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
    replace_body(Term, _, (do_remove_call(Term, Call), Expander),
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
    replace_last_literal(Conj, Conj2, CLit, CBody),
    replace_last_literal(Repl, Repl2, RLit, RBody),
    copy_term(t(Conj2, CLit, CBody, Repl2, RLit, RBody), Term),
    replace(body_rec, Conj2, Repl2,
	    ( bind_lit_body(Term, CLit, CBody, RLit, RBody),
	      Expander
	    ), Options).

replace_conjunction(Conj, Replacement, Options) :-
    replace_conjunction(Conj, Replacement, true, Options).

replace_last_literal(Conj, Body, Conj, Body) :- var(Conj), !.
replace_last_literal((A,Conj), (A,Conj2), Lit, Body) :- !,
    replace_last_literal(Conj, Conj2, Lit, Body).
replace_last_literal(Conj, Body, Conj, Body).

bind_lit_body(Term, CLit, CBody, RLit, RBody) :-
    ( subsumes_term(CLit, CBody) ->
      CBody = CLit,
      RBody = RLit,
      PCBody = PCLit,
      PRBody = PRLit
    ; subsumes_term((CLit, Rest), CBody) ->
      CBody = (CLit, Rest),
      RBody = (RLit, Rest),
      PCBody = (PCLit, PRest),
      PRBody = (PRLit, PRest)
    ),
    Term = t(Conj, PCLit, PCBody, Repl, PRLit, PRBody),
    refactor_context(pattern, Conj),
    refactor_context(into,    Repl).
