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
		     rename_predicate/3]).

:- use_module(library(readutil)).
:- use_module(library(prolog_codewalk)).
:- use_module(library(file_changes)).
:- use_module(library(term_info)).

:- dynamic file_commands_db/2.

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

% :- regtype t_action/1.
% t_action(save).
% t_action(show).

% :- pred rename_variable(?Term,+OldName:atom,+NewName:atom,+Action:t_action).

rename_variable(Caller, OldName, NewName, Action) :-
    refactor(meta_expansion(term, Caller, _,
			    rename_variable_helper(OldName, NewName)), Action).

rename_variable_helper(OldName, NewName, _, T, Dict, _, E) :-
    \+ memberchk(NewName=_,Dict),
    memberchk(OldName=V,Dict),
    V==T,
    E='$VAR'(NewName).

:- meta_predicate replace_term(?,?,?,+).
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
    expand_term(_:Term, Term,
		rename_predicate_helper(P0, H0,H), % Replace heads
		Action),
    replace_term(M:_, Name0/Arity, Name/Arity, Action). % Replace PIs

rename_predicate_helper(P0, H0, H, _:T0, T, _, P, E) :-
    nonvar(T),
    T==T0,
    ( T = P0 ->
      P = H0, E = H
    ; T = (P0 :- _) ->
      P = (H0 :- B), E = (H :- B)
    ; T = (M:P0 :- B) ->
      P = (M:H0 = B), E = (M:H :- B)
    ).

level_term(goal, _:Term, Term) :- !.
level_term(_,      Term, Term).

replace(Level, Caller0, Ref0, Expansion, Action) :-
    level_term(Level, Ref0, Term),
    copy_term(Caller0-Ref0, Caller-Ref),
    refactor(meta_expansion(Level, Caller, Ref,
			    source_expansion_helper(Term, Expansion)),
		Action).

replace_term(Caller, Term, Expansion, Action) :-
    replace(term, Caller, Term, Expansion, Action).

replace_sentence(M:Term, Expansion, Action) :-
    replace(sent, M:Term, Term, Expansion, Action).

:- meta_predicate replace_goal(?,0,?,+).
replace_goal(Caller, Term, Expansion, Action) :-
    replace(goal, Caller, Term, Expansion, Action).

:- meta_predicate expand(+,?,?,5,-).
% Expander(+Caller, ?Term, +Dict, -Pattern, -Expansion)
expand(Level, Caller, Term, Into, Expander, Action) :-
    refactor(meta_expansion(Level, Caller, Term, Into, Expander), Action).

expand_term(Caller, Term, Into, Expander, Action) :-
    expand(term, Caller, Term, Into, Expander, Action).

% Expander(+Dict, +Term, -Pattern, -Expansion)
expand_sentence(M:Term, Into, Expander, Action) :-
    expand(sent, M:Term, Term, Into, expand_sentence_helper(Expander), Action).

:- meta_predicate expand_sentence_helper(4,+,+,+,-,-).
expand_sentence_helper(Expander, M:Term, Term, Dict, Pattern, Expansion) :-
    call(Expander, M:Term, Dict, Pattern, Expansion).

expand_goal(Caller, Goal, Into, Expander, Action) :-
    expand(goal, Caller, Goal, Into, Expander, Action).

% NOTE: Only works if exactly one clause match
unfold_goal(Module, MGoal, Action) :-
    findall(clause(MGoal, Body0), clause(MGoal, Body0), [clause(MGoal, Body0)]),
    MGoal = M:_,
    (Module == M -> Body = Body0 ; Body = M:Body),
    replace_goal(MGoal, Module:_, Body, Action).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RULES (1st argument of refactor/2):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate meta_expansion(+,?,?,-,0,-).
% Expander(+Term, +Dict, +Caller, -Pattern, -Expansion)
meta_expansion(Level, Caller, Term, Into, Expander, FileChanges) :-
    collect_expansion_commands(Level, Caller, Term, Into, Expander,
			       FileCommands),
    apply_file_commands(FileCommands, FileChanges).

collect_expansion_commands(goal, Caller, Term, Into, Expander, FileCommands) :- !,
    prolog_walk_code([trace_reference(Term),
		      infer_meta_predicates(false),
		      evaluate(false),
		      on_trace(collect_file_commands(Caller, Term, Into, Expander))]),
    findall(File-Commands, retract(file_commands_db(File, Commands)), FileCommands).
collect_expansion_commands(term, Caller, Ref, Into, Expander, FileCommands) :-
    style_check(-atom),
    _:Term = Caller,
    findall(File-Commands,
	    get_file_commands(substitute_term_rec(Term, 1200, Ref, Caller, Into, Expander),
			      Caller, File, Commands),
	    FileCommands).
collect_expansion_commands(sent, Caller, Ref, Into, Expander, FileCommands) :-
    style_check(-atom),
    findall(File-Commands,
	    get_file_commands(substitute_term(1200, Ref, Caller, Into, Expander),
			      Caller, File, Commands),
	    FileCommands).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ANCILLARY PREDICATES:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

source_expansion_helper(Pattern, Expansion, _, _, _, Pattern, Expansion).

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
    sort(List, Sorted),
    collapse_list(Sorted, Uniques).

apply_commands(File-Commands, File-Changes) :-
    read_file_to_codes(File, Text, []),
    apply_pos_changes(Commands, Text, File,
		      text_desc(0, Text, Changes),
		      text_desc(_, Remaining, Remaining)).

:- meta_predicate get_file_commands(4,?,5,-,-).
get_file_commands(Substituter, M:Term, File, Commands) :-
    current_module(M),
    M \= user,
    module_property(M, class(user)),
    get_term_info(M, Term, File,
		  [variable_names(Dict), subterm_positions(TermPos)]),
    call(Substituter, Dict, TermPos, Commands, []).

:- meta_predicate substitute_term_rec(+,+,?,+,5,+,+,?,?).

substitute_term_rec(Term, Priority, Ref, Caller, Expander, Dict, TermPos) -->
    {subsumes_term(Ref, Term), Ref = Term},
    substitute_term(Priority, Term, Caller, Expander, Dict, TermPos),
    !.
substitute_term_rec(Term, _, Ref, Caller, Expander, Dict, TermPos) -->
    substitute_term_into(TermPos, Term, Dict, Caller, Ref, Expander).

:- meta_predicate substitute_term_into(+,?,+,?,?,5,?,?).
substitute_term_into(term_position(_, _, _, _, CP), Term, Dict, Caller, Ref,
		     Expander) --> !,
    substitute_term_args(CP, 1, Term, Dict, Caller, Ref, Expander).
substitute_term_into(list_position(_, _, EP, TP), Term, Dict, Caller, Ref,
		     Expander) --> !,
    substitute_term_list(EP, TP, Term, Dict, Caller, Ref, Expander).

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
substitute_term_args([PA|PAs], N0, Term, Dict, Caller, Ref, Expander) -->
    ( {arg(N0, Term, Arg)},
      {term_priority(Term, N0, Priority)},
      substitute_term_rec(Arg, Priority, Ref, Caller, Expander, Dict, PA)
    ; {N is N0 + 1},
      substitute_term_args(PAs, N, Term, Dict, Caller, Ref, Expander)
    ).

:- meta_predicate substitute_term_list(?,?,?,+,?,?,5,?,?).
substitute_term_list([EP|EPs], TP, [Elem|Term], Dict, Caller, Ref, Expander) -->
    ( {term_priority([_|_], 1, Priority)},
      substitute_term_rec(Elem, Priority, Ref, Caller, Expander, Dict, EP)
    ; substitute_term_list(EPs, TP, Term, Dict, Caller, Ref, Expander)
    ).
substitute_term_list([], TP, Tail, Dict, Caller, Ref, Expander) -->
    {term_priority([_|_], 2, Priority)},
    substitute_term_rec(Tail, Priority, Ref, Caller, Expander, Dict, TP).

:- public collect_file_commands/7.
% NOTE: Goal and Caller unified here to improve performance -- EMM
:- meta_predicate collect_file_commands(?,?,0,?,?,?).
collect_file_commands(Caller, Term, Into, Expander, Callee, Caller, From) :-
    Dict = [],			% TODO: Calculate Dict
    Term = Callee,
    calculate_commands(Expander, Callee, Into, Dict, From, File, Commands, []),
    assertz(file_commands_db(File, Commands)).

:- multifile
    prolog:message//1,
    prolog:message_location//1.

prolog:message(acheck(refactor(Goal, From))) -->
    prolog:message_location(From),
    ['Unable to refactor ~w, no term position information available'-[Goal], nl].

:- meta_predicate calculate_commands(4,?,?,?,?,?,?,?).
calculate_commands(Expander, M:Term, Into, Dict, From, File) -->
    { From = clause_term_position(ClauseRef, TermPos) ->
      clause_property(ClauseRef, file(File))
    ; From = file_term_position(File, TermPos) -> true
    ; print_message(error, acheck(refactor(M:Term, From))),
      fail
    },
    substitute_term(1200, Term, Into, Expander, Dict, TermPos).

:- meta_predicate substitute_term(+,?,+,5,+,+,?,?).

substitute_term(Priority, Term, Expansion, Expander, Dict, TermPos) -->
    {calculate_expansion(Expander, Term, Dict,
			 TermPos, Pattern)},
    expansion_commands_term(TermPos, Term, Priority, Pattern, Expansion).

:- meta_predicate calculate_expansion(5, ?, ?, ?, ?, -, -).
calculate_expansion(Expander, Term, Dict, TermPos, Pattern) :-
	setup_call_cleanup(
	    b_setval(refactor_variable_names, Dict),
	    call(Expander),
	    nb_delete(refactor_variable_names)),
	subst_term(TermPos, Pattern, Term).

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

expansion_commands_term(term_position(_, _, FFrom, FTo, SubPos),
			Term, Priority, Pattern, Expansion) -->
    { nonvar(Pattern),
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
expansion_commands_term(list_position(_, _, Elms, Tail), Term, _, Pattern, Expansion) -->
    {term_priority([_|_], 1, Priority)},
    expansion_commands_list(Elms, Tail, Term, Priority, Pattern, Expansion),
    !.
expansion_commands_term(none, _, _, _, _) --> !, [].
expansion_commands_term(TermPos, _, Priority, Pattern, Expansion) --> % Overwrite layout
    { arg(1, TermPos, From),	% BUG: can drop comments
      arg(2, TermPos, To)
    },
    ( {Pattern == Expansion} -> []
    ; [From-[print(Priority, Expansion, To)]]
    ).

expansion_commands_list([], Tail, Term, _, Pattern, Expansion) -->
    {term_priority([_|_], 2, Priority)},
    expansion_commands_term(Tail, Term, Priority, Pattern, Expansion).
expansion_commands_list([Pos|Poss], Tail, [T|Ts], Priority, [P|Ps], Expansion) -->
    { nonvar(Expansion),
      Expansion = [E|Es]
    },
    expansion_commands_term(Pos, T, Priority, P, E),
    expansion_commands_list(Poss, Tail, Ts, Priority, Ps, Es).

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

subst_term(Pos, Var, Term) :- var(Var), !, Var = '$substitute_by'(Pos, Term).
subst_term(_, '$substitute_by'(_, _), _) :- !. %Avoid aliasing loops
subst_term(term_position(_, _, _, _, CP), Term, CTerm) :- !,
    subst_args(1, Term, CTerm, CP).
subst_term(list_position(_, _, Elms, Tail), Term, CTerm) :- !,
    subst_list(Elms, Tail, Term, CTerm).
subst_term(none, T, T) :- !.
subst_term(_, T, T).

collapse_list([], []).
collapse_list([E|L], R) :-
    collapse_list2(L, E, R).

collapse_list2([], E, [E]).
collapse_list2([E|L], E0, R0) :-
    collapse_elem(E, E0, R0, R),
    collapse_list2(L, E, R).

collapse_elem(E, E, R,     R) :- !.
collapse_elem(_, E, [E|R], R).

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
