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
	  [replace_term/4,
	   replace_head/4,
	   replace_body/4,
	   replace_goal/4,
	   replace_sentence/3,
	   rename_variable/4,
	   replace_term_id/4,
	   unfold_goal/2,
	   rename_predicate/3,
	   rename_functor/4,
	   remove_useless_exports/1,
	   replace_conjunction/4,
	   replace_conjunction/5,
	   remove_call/3,
	   remove_call/4
	  ]).

:- use_module(library(ref_context)).
:- use_module(library(ref_expanders)).
:- use_module(library(list_sequence)).

:- meta_predicate
	unfold_goal(0,+).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Most used refactoring scenarios:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_useless_exports(Options0) :-
    select_option(module(M), Options0, Options, M),
    expand_sentence((:-module(M,L)), (:- module(M,N)),
		    ( include(being_used(M), L, N),
		      L \= N
		    ), [module(M)|Options]),
    expand_sentence((:-export(K)), Exp,
		    ( once(list_sequence(L, K)),
		      include(being_used(M), L, N),
		      ( N = []                   -> Exp = []
		      ; L \= N, list_sequence(N,S), Exp = (:- export(S))
		      )
		    ), [module(M)|Options]).

being_used(M, F/A) :-
    functor(H, F, A),
    predicate_property(C:H, imported_from(M)), C \== user.

% :- type t_action/1.
% t_action(save).
% t_action(show).

%% rename_variable(?Sentence, +Name0:atom, +Name:atom, +Action:t_action) is det.
%
rename_variable(Sent, Name0, Name, Options) :-
    expand_sentence(Sent, Sent,
		    ( \+ memberchk(Name =_, Dict),
		      memberchk(Name0='$VAR'(Name), Dict)
		    ), [variable_names(Dict)|Options]).

rename_functor(Sentence, Functor/Arity, NewName, Options) :-
    functor(Term, Functor, Arity),
    Term =.. [_|Args],
    Expansion =.. [NewName|Args],
    replace_term_id(Sentence, Term, Expansion, Options).

replace_term_id(Sentence, Term, Expansion, Options) :-
    replace_term(Sentence, Term, Expansion, Options),
    functor(Term, F0, A0),
    functor(Expansion, F, A),
    replace_term(Sentence, F0/A0, F/A, Options).

% BUG: This have to be applied only once --EMM
:- meta_predicate rename_predicate(+,+,+).
rename_predicate(M:Name0/Arity, Name, Options) :-
    functor(H0, Name0, Arity),
    H0 =.. [Name0|Args],
    H  =.. [Name|Args],
    replace_goal(_, M:H0, H, Options), % Replace calls
    % Replace heads:
    expand_sentence((  H0 :- B),   (H :- B), true, Options),
    expand_sentence((M:H0 :- B), (M:H :- B), true, Options),
    expand_sentence(H0, H, true, Options),
    expand_sentence((M:H0), (M:H), true, Options),
    replace_term(_, Name0/Arity, Name/Arity, [module(M)|Options]). % Replace PIs

:- meta_predicate replace_term(?,?,?,+).
replace_term(Sentence, Term, Expansion, Options) :-
    expand_term(Sentence, Term, Expansion, true, Options).

replace_head(Sentence, Term, Expansion, Options) :-
    expand_head(Sentence, Term, Expansion, true, Options).

replace_body(Sentence, Term, Expansion, Options) :-
    expand_body(Sentence, Term, Expansion, true, Options).

replace_sentence(Sentence, Expansion, Options) :-
    expand_sentence(Sentence, Expansion, true, Options).

:- meta_predicate replace_goal(?,?,?,+).
replace_goal(Sentence, Term, Expansion, Options) :-
    expand_goal(Sentence, Term, Expansion, true, Options).

% NOTE: Only works if exactly one clause match
unfold_goal(MGoal, Options) :-
    findall(clause(MGoal, Body0), clause(MGoal, Body0), [clause(MGoal, Body0)]),
    MGoal = M:Goal,
    expand_goal(_, Goal, Body,
		(Module == M -> Body = Body0 ; Body = M:Body0),
		[module(Module)|Options]).

:- meta_predicate remove_call(+,+,0,+).
remove_call(Sentence, Call, Expander, Options) :-
    expand_body(Sentence, Term, _, (do_remove_call(Term, Call), Expander),
		Options).

remove_call(Sentence, Call, Options) :-
    remove_call(Sentence, Call, true, Options).

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

:- meta_predicate replace_conjunction(?, ?, ?, 0, +).
replace_conjunction(Sentence, Conj, Repl, Expander, Options) :-
    replace_last_literal(Conj, Conj2, CLit, CBody),
    replace_last_literal(Repl, Repl2, RLit, RBody),
    copy_term(t(Conj2, CLit, CBody, Repl2, RLit, RBody), Term),
    expand_term(Sentence, Conj2, Repl2,
		( bind_lit_body(Term, CLit, CBody, RLit, RBody),
		  Expander
		), Options).

replace_conjunction(Sentence, Conj, Replacement, Options) :-
    replace_conjunction(Sentence, Conj, Replacement, true, Options).

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
