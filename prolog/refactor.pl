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

:- module(refactor, [refactor/2,
		     rename_variable/4,
		     replace_term/4,
		     replace_goal/4,
		     replace_sentence/3,
		     expand_term/5,
		     expand_goal/5,
		     expand_sentence/4,
		     replace_term_id/4,
		     unfold_goal/3,
		     rename_predicate/3,
		     rename_functor/4,
		     refactor_context/2,
		     remove_useless_exports/2,
		     replace_conjunction/4
		    ]).

:- use_module(library(readutil)).
:- use_module(library(prolog_codewalk)).
:- use_module(library(file_changes)).
:- use_module(library(term_info)).

:- thread_local file_commands_db/2.

:- meta_predicate
	refactor(1,+),
	expand_term(+,+,-,0,+),
	expand_sentence(+,-,0,+),
	expand_goal(?,:,-,0,+),
	unfold_goal(?,0,+).

refactor(Rule, Action) :-
    call(Rule, FileChanges),
    do_file_changes(Action, FileChanges).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Most used refactoring scenarios:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_useless_exports(Module, Action) :-
    expand_sentence(Module:(:-module(M,L)), (:- module(M,N)),
		    include(being_used(Module), L, N), Action),
    expand_sentence(Module:(:-export(K)), Exp,
		    ( once(list_sequence(L, K)),
		      include(being_used(Module), L, N),
		      ( N = []           -> Exp = '$RM'
		      ; list_sequence(N,S), Exp = (:- export(S))
		      )
		    ), Action).

list_sequence([], []).
list_sequence([E|L], S) :- list_sequence_2(L, E, S).

list_sequence_2([E|L], E0, (E0, S)) :- list_sequence_2(L, E, S).
list_sequence_2([], E, E).

being_used(M, F/A) :-
    functor(H, F, A),
    predicate_property(C:H, imported_from(M)), C \== user.

% :- regtype t_action/1.
% t_action(save).
% t_action(show).

%% rename_variable(?Sentence, +Name0:atom, +Name:atom, +Action:t_action) is det.

rename_variable(MSent,Name0,Name,Action) :-
    expand_term(MSent,Var,'$VAR'(Name),
		( refactor_context(variable_names, Dict),
		  \+ memberchk(Name =_,Dict),var(Var),
		  memberchk(Name0=V,Dict),V==Var
		), Action).

rename_functor(Caller, Functor/Arity, NewName, Action) :-
    functor(Term, Functor, Arity),
    Term =.. [_|Args],
    Expansion =.. [NewName|Args],
    replace_term_id(Caller, Term, Expansion, Action).
    
replace_term_id(Caller, Term, Expansion, Action) :-
    replace_term(Caller, Term, Expansion, Action),
    functor(Term, F0, A0),
    functor(Expansion, F, A),
    replace_term(Caller, F0/A0, F/A, Action).

% BUG: This have to be applied only once --EMM
:- meta_predicate rename_predicate(+,+,+).
rename_predicate(M:Name0/Arity, Name, Action) :-
    functor(H0, Name0, Arity),
    H0 =.. [Name0|Args],
    H  =.. [Name|Args],
    functor(P0, Name0, Arity),
    replace_goal(_, M:H0, H, Action), % Replace calls
    expand_term(_:Term, Term, Into,
		rename_predicate_helper(P0,H0,H,Term,Into), % Replace heads
		Action),
    replace_term(M:_, Name0/Arity, Name/Arity, Action). % Replace PIs

rename_predicate_helper(P0, H0, H, P, E) :-
%    nonvar(T),
%    T==T0,
    ( T = P0 ->
      P = H0, E = H
    ; T = (P0 :- _) ->
      P = (H0 :- B), E = (H :- B)
    ; T = (M:P0 :- B) ->
      P = (M:H0 = B), E = (M:H :- B)
    ).

replace(Level, Sentence, From, Into, Action) :-
    expand(Level, Sentence, From, Into, true, Action).

:- meta_predicate replace_term(?,?,?,+).
replace_term(Caller, Term, Expansion, Action) :-
    replace(term, Caller, Term, Expansion, Action).

replace_sentence(M:Term, Expansion, Action) :-
    replace(sent, M:Term, Term, Expansion, Action).

:- meta_predicate replace_goal(?,0,?,+).
replace_goal(Caller, Term, Expansion, Action) :-
    replace(goal, Caller, Term, Expansion, Action).

:- meta_predicate expand(+,?,?,?,0,-).
% Expander(+Caller, ?Term, +Dict, -Pattern, -Expansion)
expand(Level, Caller, Term, Into, Expander, Action) :-
    refactor(meta_expansion(Level, Caller, Term, Into, Expander), Action).

%%	expand_term(?Sentence, ?Term, ?Replacement, :Expander, +Action)

expand_term(Caller, Term, Into, Expander, Action) :-
    expand(term, Caller, Term, Into, Expander, Action).

%%	expand_sentence(?Sentence, ?Into, :Expander, +Action).

expand_sentence(M:Term, Into, Expander, Action) :-
    expand(sent, M:Term, Term, Into, Expander, Action).

expand_goal(Caller, Goal, Into, Expander, Action) :-
    expand(goal, Caller, Goal, Into, Expander, Action).

% NOTE: Only works if exactly one clause match
unfold_goal(Module, MGoal, Action) :-
    findall(clause(MGoal, Body0), clause(MGoal, Body0), [clause(MGoal, Body0)]),
    MGoal = M:_,
    (Module == M -> Body = Body0 ; Body = M:Body),
    replace_goal(Module:_, MGoal, Body, Action).

replace_conjunction(Sentence, Conj, Replacement, Action) :-
    expand_term(Sentence, Conj, Replacement, true, Action),
    extend_conj(Conj, Rest, Conj2),
    extend_conj(Replacement, Rest, Replacement2),
    expand_term(Sentence, Conj2, Replacement2, true, Action).

extend_conj(Var, Rest, (Var,Rest)) :- var(Var), !.
extend_conj((A,C0), Rest, (A,C)) :- !, extend_conj(C0, Rest, C).
extend_conj(Last, Rest, (Last,Rest)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RULES (1st argument of refactor/2):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate meta_expansion(+,?,?,-,0,-).

%%	meta_expansion(+Level, +Sentence, +Term, +Into,
%%		       :Expander, -FileChanges) is det
%
%	Expand  terms  that  subsume  Term  in  sentences  that  subsume
%	Sentence into Into if Expander is true.

meta_expansion(Level, Caller, Term, Into, Expander, FileChanges) :-
    collect_expansion_commands(Level, Caller, Term, Into, Expander,
			       FileCommands),
    apply_file_commands(FileCommands, FileChanges).

collect_expansion_commands(goal, Caller, Term, Into, Expander, FileCommands) :-
    prolog_walk_code(
	[ trace_reference(Term),
	  infer_meta_predicates(false),
	  evaluate(false),
	  on_trace(collect_file_commands(Caller, Term, Into, Expander))
	]),
    findall(File-Commands,
	    retract(file_commands_db(File, Commands)),
	    FileCommands).
collect_expansion_commands(term, Caller, Ref, Into, Expander, FileCommands) :-
    style_check(-atom),
    M:SentencePattern = Caller,
    findall(File-Commands,
	    ( refactor_module(M),
	      get_term_info(M, SentencePattern, Sentence, File,
			    [ variable_names(Dict),
			      subterm_positions(TermPos)
			    ]),
	      phrase(substitute_term_rec(Sentence, 1200,
					 Ref, Into, Expander, Dict, TermPos),
		     Commands)
	    ),
	    FileCommands).
collect_expansion_commands(sent, Caller, Ref, Into, Expander, FileCommands) :-
    style_check(-atom),
    M:SentencePattern = Caller,
    findall(File-Commands,
	    ( refactor_module(M),
	      get_term_info(M, SentencePattern, Sentence, File,
			    [ variable_names(Dict),
			      subterm_positions(TermPos)
			    ]),
	      phrase(substitute_term_norec(Sentence, 1200,
					   Ref, Into, Expander, Dict, TermPos),
		     Commands)
	    ),
	    FileCommands).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ANCILLARY PREDICATES:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apply_file_commands(Pairs, FileChanges) :-
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    findall(Compact, ( member(Group, Grouped),
		       compact_group(Group, Compact)),
	    Compacted),
    maplist(apply_commands, Compacted, FileChanges).

compact_group(Key-LList, Key-Uniques) :-
    append(LList, List),
    List \== [],
    sort(List, Uniques).

apply_commands(File-Commands, File-Changes) :-
    read_file_to_codes(File, Text, []),
    apply_pos_changes(Commands, Text, File,
		      text_desc(0, Text, Changes),
		      text_desc(_, Remaining, Remaining)).

%%	refactor_module(?M)
%
%	True when M is a module we should refactor.

refactor_module(M) :-
	current_module(M),
	M \= user,				% Dubious --JW
	module_property(M, class(user)).


%%	substitute_term_norec(+Term, +Priority, +Pattern, -Into, :Expander, +Dict, +TermPos)// is nondet.
%
%	None-recursive version of substitute_term_rec//7.

substitute_term_norec(Term, Priority, Pattern, Into,
		      Expander, Dict, TermPos) -->
	{ subsumes_term(Pattern, Term),
	  with_context(Dict, Term, Pattern, Pattern2, Into, Into2, Expander)
	},
	substitute_term(Priority, Term, Pattern2, Into2, TermPos), !.


%%	refactor_context(?Name, ?Value) is nondet.

refactor_context(variable_names, Bindings) :-
	b_getval(refactor_variable_names, Bindings).

:- meta_predicate
	with_context(+, +, +, -, +, -, 0).

with_context(Dict, Src, Pattern0, Pattern, Into0, Into, Goal) :-
	copy_term(Pattern0, Pattern),
	term_variables(Pattern0, Vars0),
	term_variables(Pattern, Vars),
	Pattern0 = Src,
	setup_call_cleanup(
	    ( b_setval(refactor_variable_names, Dict)
	    ),
	    call(Goal),
	    ( nb_delete(refactor_variable_names)
	    )),
	pairs_keys_values(Pairs, Vars0, Vars),
	map_subterms(Pairs, Into0, Into).

map_subterms(Pairs, T0, T) :-
	member(X0-X, Pairs),
	same_term(X0, T0), !,
	T = X.
map_subterms(Pairs, T0, T) :-
	compound(T0), !,
	T0 =.. [F|Args0],
	maplist(map_subterms(Pairs), Args0, Args),
	T =.. [F|Args].
map_subterms(_, T, T).


:- meta_predicate substitute_term_rec(+,+,?,+,5,+,+,?,?).

%%	substitute_term_rec(+SrcTerm, +Priority, +Pattern, -Into, :Expander, +Dict, +TermPos)// is nondet.
%
%	True when the DCG list contains   a  substitution for Pattern by
%	Into in SrcTerm. This predicate must  be cautious about handling
%	bindings:
%
%	  - Overall bindings do not affect further substitutions because
%	    we are managed by findall/3 in collect_expansion_commands/6.
%	  - Pattern must not be instantiated by either unification with
%	    SrcTerm or the execution of Expander.  This is needed for
%	    substitute_term/7 to find the correct replacements.
%
%	To avoid binding Pattern, we need to copy Pattern and Into while
%	maintaining sharing with Expander.  Next,   we  can safely unify
%	Pattern with the SrcTerm.

substitute_term_rec(Term, Priority, Pattern, Into, Expander, Dict, TermPos) -->
	{ subsumes_term(Pattern, Term),
	  with_context(Dict, Term, Pattern, Pattern2, Into, Into2, Expander)
	},
	substitute_term(Priority, Term, Pattern2, Into2, TermPos), !.
substitute_term_rec(Term, _, Ref, Into, Expander, Dict, TermPos) -->
	substitute_term_into(TermPos, Term, Dict, Ref, Into, Expander).

:- meta_predicate substitute_term_into(+,?,+,?,?,5,?,?).
substitute_term_into(term_position(_, _, _, _, CP), Term, Dict, Ref,
		     Into, Expander) --> !,
    substitute_term_args(CP, 1, Term, Dict, Ref, Into, Expander).
substitute_term_into(list_position(_, _, EP, TP), Term, Dict, Ref,
		     Into, Expander) --> !,
    substitute_term_list(EP, TP, Term, Dict, Ref, Into, Expander).

:- use_module(library(listing), []).

term_priority(Term, N, Priority) :-
    nonvar(Term),
    functor(Term, F, A),
    ( ( A == 1 ->
	( prolog_listing:prefix_op(F, Priority) -> true
	; prolog_listing:postfix_op(F, Priority) -> true
	)
      ; A == 2 ->
	prolog_listing:infix_op(F, Left, Right),
	( N==1 -> Priority = Left
	; N==2 -> Priority = Right
	)
      ) -> true
    ; Priority = 1200
    ).

:- meta_predicate substitute_term_args(?,+,?,?,+,?,?,5,?,?).
substitute_term_args([PA|PAs], N0, Term, Dict, Ref, Into, Expander) -->
    ( {arg(N0, Term, Arg)},
      {term_priority(Term, N0, Priority)},
      substitute_term_rec(Arg, Priority, Ref, Into, Expander, Dict, PA)
    ; {N is N0 + 1},
      substitute_term_args(PAs, N, Term, Dict, Ref, Into, Expander)
    ).

:- meta_predicate substitute_term_list(?,?,?,+,?,?,5,?,?).
substitute_term_list([EP|EPs], TP, [Elem|Term], Dict, Ref, Into, Expander) -->
    ( {term_priority([_|_], 1, Priority)},
      substitute_term_rec(Elem, Priority, Ref, Into, Expander, Dict, EP)
    ; substitute_term_list(EPs, TP, Term, Dict, Ref, Into, Expander)
    ).
substitute_term_list([], TP, Tail, Dict, Ref, Into, Expander) -->
    {term_priority([_|_], 2, Priority)},
    substitute_term_rec(Tail, Priority, Ref, Into, Expander, Dict, TP).

:- public collect_file_commands/7.
:- meta_predicate collect_file_commands(?,?,0,?,?,?).

%%	collect_file_commands(+Sentence, +Pattern, +Into, :Expander,
%%			      +Callee, +Caller, +Location)
%
%	Called from prolog_walk_code/1 on a call  from Caller to Callee.
%	The parameters Sentence to Expander are provided by the on_trace
%	closure  passed  to  prolog_walk_code/1.    Callee,  Caller  and
%	Location are added by prolog_walk_code/1.

collect_file_commands(CallerPattern, Pattern, Into, Expander,
		      Callee, Caller, From) :-
	subsumes_term(CallerPattern, Caller),
	with_context([], Callee, Pattern, Pattern2, Into, Into2, Expander),
	calculate_commands(Callee, Pattern2, Into2, From, File, Commands, []),
	assertz(file_commands_db(File, Commands)).

:- multifile
    prolog:message//1,
    prolog:message_location//1.

prolog:message(acheck(refactor(Goal, From))) -->
    prolog:message_location(From),
    ['Unable to refactor ~w, no term position information available'-[Goal], nl].

calculate_commands(M:Term, M:Pattern, Into, From, File) -->
    { From = clause_term_position(ClauseRef, TermPos) ->
      clause_property(ClauseRef, file(File))
    ; From = file_term_position(File, TermPos) -> true
    ; print_message(error, acheck(refactor(M:Term, From))),
      fail
    },
    substitute_term(1200, Term, Pattern, Into, TermPos).

:- meta_predicate substitute_term(+,?,+,5,+,+,?,?).

%%	substitute_term(+Priority, +SrcTerm, +Pattern, +Into, +TermPos)
%
%	Substitute occurences of Pattern with Into after calling
%	expansion.
%
%	@param SrcTerm is the term as read from the source
%	@param Dict is the variable name assignment in SrcTerm
%	@param TermPos is the term layout of SrcTerm
%	@param Priority is the environment operator priority

substitute_term(Priority, Term, Pattern, Into, TermPos) -->
    { subst_term(TermPos, Pattern, Term)
    },
    expansion_commands_term(TermPos, Term, Priority, Pattern, Into).

valid_op_type_arity(xf,  1).
valid_op_type_arity(yf,  1).
valid_op_type_arity(xfx, 2).
valid_op_type_arity(xfy, 2).
valid_op_type_arity(yfx, 2).
valid_op_type_arity(fy,  1).
valid_op_type_arity(fx,  1).

refactor_hack('$LIST'(_)).
refactor_hack('$TEXT'(_)).
refactor_hack('$TEXT'(_,_)).
refactor_hack('$BODY'(_)).
refactor_hack('$NL'(_)).
refactor_hack('$,NL'(_)).
refactor_hack('$RM').

o_length(T, N) :-
    o_length(T, 0, N).

o_length(V, N, N) :- var(V), !.
o_length([_|T], N0, N) :- !,
    N1 is N0 + 1,
    o_length(T, N1, N).
o_length(_, N, N).

expansion_commands_term(term_position(_, _, FFrom, FTo, SubPos),
			Term, Priority, Pattern, Expansion) -->
    { nonvar(Pattern),
      Term \== Expansion,
      Pattern \= '$substitute_by'(_, _),
      nonvar(Expansion),
      Expansion \= '$substitute_by'(_, _),
      \+ refactor_hack(Expansion),
      functor(Pattern, FP, A),
      functor(Expansion, FE, A),
      ( valid_op_type_arity(TypeOp, A),
	current_op(PrecedenceP, TypeOp, FP),
	current_op(PrecedenceE, TypeOp, FE),
	PrecedenceP >= PrecedenceE
      ; \+ current_op(PrecedenceP, _, FP),
	\+ current_op(PrecedenceE, _, FE)
      )
    },
    !,
    ( {FP\=FE} ->
      [FFrom-[print(Priority, FE, FTo)]]
    ; [] %% Do nothing to preserve functor layout
    ),
    expansion_commands_args(1, Term, Pattern, Expansion, SubPos).
expansion_commands_term(list_position(_, _, Elms, TailPos), Term, _, Pattern, Expansion) -->
    { o_length(Pattern,   N),
      o_length(Expansion, N),
      term_priority([_|_], 1, Priority)
    },
    expansion_commands_list(Elms, TailPos, Term, Priority, Pattern, Expansion),
    !.
expansion_commands_term(none, _, _, _, _) --> !, [].
expansion_commands_term(TermPos, _, Priority, Pattern, Expansion) --> % Overwrite layout
    { arg(1, TermPos, From),	% BUG: can drop comments
      arg(2, TermPos, To)
    },
    ( {Pattern == Expansion} -> []
    ; [From-[print(Priority, Expansion, To)]]
    ).

expansion_commands_list(_TermPos, _TailPos, Term, _, _Pattern, Expansion) -->
    {Term == Expansion}, !, [].
expansion_commands_list([], TailPos, Term, _, Pattern, Expansion) -->
    {term_priority([_|_], 2, Priority)},
    !,
    expansion_commands_term(TailPos, Term, Priority, Pattern, Expansion).
expansion_commands_list([Pos|Poss], TailPos, Term, Priority, [P|Ps], Expansion) -->
    { nonvar(Expansion),
      Expansion = [E|Es], Term = [T|Ts]
    },
    !,
    expansion_commands_term(Pos,  T,           Priority, P,  E),
    expansion_commands_list(Poss, TailPos, Ts, Priority, Ps, Es).

expansion_commands_args(N, Term, Pattern, Expansion, [ArgPos|SubPos]) -->
    { arg(N, Pattern, PArg), !,
      arg(N, Expansion, EArg),
      arg(N, Term, TArg),
      term_priority(Term, N, Priority)
    },
    expansion_commands_term(ArgPos, TArg, Priority, PArg, EArg),
    {succ(N, N1)},
    expansion_commands_args(N1, Term, Pattern, Expansion, SubPos).
expansion_commands_args(_, _, _, _, _) --> [].

subst_args(N, Pattern, Term, [ArgPos|SubPos]) :-
    arg(N, Pattern, PArg), !,
    arg(N, Term, Arg),
    subst_term(ArgPos, PArg, Arg),
    succ(N, N1),
    subst_args(N1, Pattern, Term, SubPos).
subst_args(_, _, _, _).

subst_list([], Tail, E, C) :-
    subst_term(Tail, E, C).
subst_list([Pos|Poss], Tail, [E|Es], [C|Cs]) :-
    subst_term(Pos, E, C),
    subst_list(Poss, Tail, Es, Cs).

%%	subst_term(+Position, +Pattern, +Term)
%
%	Here, Pattern is a term  that   holds  variables.  It is matched
%	against a position term and  if  there   is  a  variable  in the
%	pattern, this is unified to   a  '$substitute_by'(Pos, SubTerm),
%	indicating that this position currently holds SubTerm.
%
%	@param Position is a subterm-position term for Term
%	@param Term is a source term
%	@param Pattern is a substitution pattern

subst_term(Pos, Var, Term) :- var(Var), !, Var = '$substitute_by'(Pos, Term).
subst_term(_, '$substitute_by'(_, _), _) :- !. %Avoid aliasing loops
subst_term(term_position(_, _, _, _, CP), Term, CTerm) :- !,
    subst_args(1, Term, CTerm, CP).
subst_term(list_position(_, _, Elms, Tail), Term, CTerm) :- !,
    subst_list(Elms, Tail, Term, CTerm).
subst_term(none, T, T) :- !.
subst_term(_, T, T).

apply_pos_changes([], _, _) --> [].
apply_pos_changes([Pos-Changes|PosChanges], Text, File) -->
    apply_changes(Changes, Pos, Text, File),
    apply_pos_changes(PosChanges, Text, File).

apply_changes([], _, _, _) --> [].
apply_changes([Change|Changes], Pos, Text, File) -->
    apply_change(Change, Pos, Text, File),
    apply_changes(Changes, Pos, Text, File).

cut_text(Pos0, Pos, Remaining0, Remaining, Text) :-
    ( Pos > Pos0 ->
      Seek is Pos - Pos0,
      length(Text, Seek),
      append(Text, Remaining, Remaining0)
    ; Remaining0 = Remaining,
      Text = ""
    ).

% BUG: printing of parenthesis if required must be done consulting
% current_op/3 and taking actions.
rportray(Pos0-Remaining0, '$substitute_by'(ArgPos, Term), Opt) :-
    arg(1, ArgPos, From),
    arg(2, ArgPos, To),
    cut_text(Pos0, From, Remaining0, Remaining1, _),
    cut_text(From, To, Remaining1, _, Text),
    memberchk(priority(N),Opt),
    ( prolog_listing:term_needs_braces(Term, N) ->
      format('(~s)', [Text])
    ; format('~s', [Text])
    ),
    !.
rportray(_, '$substitute_by'(_, _), _) :- !.
rportray(_, '$LIST'(L), Opt) :- !,
    maplist(term_write(Opt), L).
rportray(_, '$TEXT'(T), Opt0) :- !,
    subtract(Opt0, [quoted(true), portray_goal(_)], Opt),
    write_term(T, Opt).
rportray(Pos-Remaining, '$BODY'(B), Opt) :-
    memberchk(priority(N), Opt),
    write_b(B, rportray(Pos-Remaining), N, _File, Pos).

term_write(Opt, Term) :- write_term(Term, Opt).

:- use_module(library(listing),[]).

apply_change(print(Priority, Term, SkipTo), Pos1, Text, File,
	     text_desc(Pos0, Remaining0, Tail0),
	     text_desc(Pos,  Remaining,  Tail)) :-
    cut_text(Pos0, Pos1, Remaining0, Remaining1, CutText), %
    append(CutText, Tail1, Tail0),			   % Accept
    findall(t(Tail1, Tail, Pos),
	    ( numbervars(Term, 0, _, [singletons(true)]),
	      with_output_to(codes(Tail1, Tail),
			     print_expansion(Term, rportray(0-Text), Priority,
					     File, Pos1, SkipTo, Pos)
			    )),
	    [t(Tail1, Tail, Pos)]),		   % Apply
    cut_text(Pos1, Pos, Remaining1, Remaining, _). % Skip

inc(Delta, Pos0, Pos) :- Pos is Pos0 + Delta.

print_expansion_list([], _, _, _, _) --> [].
print_expansion_list([T|L], PG, N, File, Pos0) -->
    print_expansion(T, PG, N, File, Pos0),
    print_expansion_list(L, PG, N, File, Pos0).

%% print_expansion(?Term:term, N:integer, File:atom, Pos0:integer, SkipTo:integer, Pos:integer).

print_expansion(Term, PG, N, _, _) --> {var(Term), !, write_r(N, PG, Term)}, [].
print_expansion('$,NL', PG, N, File, Pos0) --> % Print a comma + indented new line
    {write(',')},
    print_expansion('$NL', PG, N, File, Pos0).
print_expansion('$BODY'(Term), PG, N, File, Pos) --> % Print as a body
    {write_b(Term, PG, N, File, Pos)}.
print_expansion('$NL', _, _, File, Pos0) --> % Print an indented new line
    { prolog_codewalk:filepos_line(File, Pos0, _, LinePos),
      nl,
      line_pos(LinePos)
    }.
print_expansion('$LIST'(List), PG, N, File, Pos0) -->
    print_expansion_list(List, PG, N, File, Pos0).
print_expansion('$TEXT'(Term), PG, N, File, Pos0) -->
    print_expansion('$TEXT'(Term, 0), PG, N, File, Pos0).
% BUG: assuming no spaces between Term, full stop and new line:
print_expansion('$RM', _, _, _, _) --> inc(2).
print_expansion('$TEXT'(Term, Delta), _, N, _, _) -->
    {write_t(N, Term)},
    inc(Delta).
print_expansion(Term, PG, N, _, _) --> {write_r(N, PG, Term)}.

bin_op(Term, Op, Left, Right, A, B) :-
    nonvar(Term),
    functor(Term, Op, N),
    N == 2,
    prolog_listing:infix_op(Op, Left, Right),
    arg(1, Term, A),
    arg(2, Term, B).

write_b(Term, PG, N, File, Pos0) :-
    ( prolog_listing:term_needs_braces(Term, N)
    -> write('( '),
      Pos is Pos0 + 2,
      write_b1(Term, PG, N, File, Pos),
      write(')')
    ; write_b1(Term, PG, N, File, Pos0)
    ).

and_layout(T) :- T = (_,_).

write_b1(Term, PG, _, File, Pos) :-
    prolog_listing:or_layout(Term), !,
    write_b_layout(Term, PG, or,  File, Pos).
write_b1(Term, PG, _, File, Pos) :-
    and_layout(Term), !,
    write_b_layout(Term, PG, and, File, Pos).
write_b1(Term, PG, N, _, _) :- write_r(N, PG, Term).

write_b_layout(Term, PG, Layout, File, Pos) :-
    bin_op(Term, Op, Left, Right, A, B),
    !,
    write_b(A, PG, Left, File, Pos),
    prolog_codewalk:filepos_line(File, Pos, _, LinePos),
    nl_indent(Layout, Op, LinePos),
    write_b(B, PG, Right, File, Pos).

nl_indent(or, Op, LinePos0) :-
    nl,
    LinePos is max(0, LinePos0 - 2),
    line_pos(LinePos),
    write(Op),
    write(' ').
nl_indent(and, Op, LinePos) :-
    write(Op),
    nl,
    line_pos(LinePos).

line_pos(0) :- !.
line_pos(LinePos) :-
    LinePos >= 8,
    !,
    write('\t'),
    LinePos1 is LinePos - 8,
    line_pos(LinePos1).
line_pos(LinePos) :-
    LinePos >= 1,
    write(' '),
    LinePos1 is LinePos - 1,
    line_pos(LinePos1).

write_t(N, Term) :-
    write_term(Term, [spacing(next_argument),
		      numbervars(true),
		      priority(N)]).

:- meta_predicate write_r(+,2,?).
write_r(N, PortrayGoal, Term) :-
    Options = [portray_goal(PortrayGoal),
	       spacing(next_argument),
	       numbervars(true),
	       quoted(true),
	       priority(N)],
    write_term(Term, Options).