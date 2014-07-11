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

:- module(ref_expand, [expand/5]).

:- use_module(library(readutil)).
:- use_module(library(file_changes)).
:- use_module(library(term_info)).
:- use_module(library(gcb)).
:- use_module(library(substitute)).
:- use_module(library(list_sequence)).
:- use_module(library(maplist_dcg)).
:- use_module(library(mapargs)).
:- use_module(library(location_utils)).
:- use_module(library(ref_changes)).
:- use_module(library(ref_context)).

:- thread_local file_commands_db/2, command_db/1.

:- multifile
    prolog:xref_open_source/2.	% +SourceId, -Stream

prolog:xref_open_source(File, Fd) :-
    once(pending_change(_, File, Source)),
    open_codes_stream(Source, Fd).

:- meta_predicate expand(+,?,?,0,-).
expand(Level, Term, Into, Expander, Options) :-
    meta_expansion(Level, Term, Into, Expander, Options, FileContent),
    save_changes(FileContent).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

curr_style(Style, CurrStyle) :-
    arg(1, Style, Name),
    ( style_check(?(Name))
    ->CurrStyle = +Name
    ; CurrStyle = -Name
    ).

:- meta_predicate with_styles(0, +).
with_styles(Goal, StyleL) :-
    maplist(curr_style, StyleL, OldStyleL),
    setup_call_cleanup(maplist(style_check, StyleL),
		       Goal,
		       maplist(style_check, OldStyleL)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RULES (1st argument of refactor/2):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate meta_expansion(+,?,-,0,+,-).

%%	meta_expansion(+Level, +Term, +Into, :Expander, -FileChanges) is det
%
%	Expand  terms  that  subsume  Term  in  sentences  that  subsume
%	Sentence into Into if Expander is true.

meta_expansion(Level, Term, Into, Expander, Options, FileContent) :-
    with_styles(collect_expansion_commands(Level, Term, Into, Expander,
					   Options, FileCommands),
		[-atom, -singleton]), % At this point we are not interested in styles
    apply_file_commands(FileCommands, FileContent).

:- public r_goal_expansion/2.

r_goal_expansion(Goal, TermPos) :-
    once(do_r_goal_expansion(Goal, TermPos)),
    fail.

do_r_goal_expansion(Term, TermPos) :-
    refactor_context(sentence, Sent),
    refactor_context(sent_pattern, SentPattern),
    subsumes_term(SentPattern, Sent),
    refactor_context(goal_args, ga(Pattern, Into, Expander)),
    phrase(substitute_term_norec(sub, Term, 999, Pattern, Into, Expander, TermPos),
	   Commands, []),
    forall(member(Command, Commands), assertz(command_db(Command))).

:- meta_predicate level_hook(+,+,-,-,0 ).
level_hook(goal, Options0, Options, Expanded, Call) :- !,
    %% In goal, expanded=yes:
    select_option(expanded(Expanded), Options0, Options, yes),
    setup_call_cleanup(asserta((system:goal_expansion(G, T, _, _) :-
			       r_goal_expansion(G, T)), Ref),
		       Call,
		       erase(Ref)).
level_hook(_, Options0, Options, Expanded, Call) :-
    select_option(expanded(Expanded), Options0, Options, no),
    call(Call).


:- public collect_file_commands/8.
:- meta_predicate collect_file_commands(?,0,?,?,?,?,?,?).

%%	collect_file_commands(+CallerPattern, +Pattern, +Into, :Expander,
%%			      +FileChk, +Callee, +Caller, +Location)
%
%	Called from prolog_walk_code/1 on a call  from Caller to Callee.
%	The parameters Sentence to Expander are provided by the on_trace
%	closure  passed  to  prolog_walk_code/1.    Callee,  Caller  and
%	Location are added by prolog_walk_code/1.

collect_file_commands(CallerPattern, Pattern, Into, Expander, FileChk,
		      Callee, Caller, From) :-
    subsumes_term(CallerPattern-Pattern, Caller-Callee),
    ( From = clause_term_position(ClauseRef, TermPos)
    ->clause_property(ClauseRef, file(File))
    ; From = file_term_position(File, TermPos)
    ->true
    ; print_message(error, acheck(refactor(Callee, From))),
      fail
    ),
    call(FileChk, File),
    Callee = M:Term,
    % trim_term(Term0, Term, TermPos0, TermPos),
    Pattern = M:Pattern2,
    with_context_vars(phrase(substitute_term_norec(sub, Term, 999, Pattern2, Into,
						   Expander, TermPos),
			     Commands, []),
		      [refactor_sent_pattern,
		       refactor_sentence,
		       refactor_goal_args],
		     [CallerPattern,
		      Caller,
		      ga(Term, Into, Expander)]),
    assertz(file_commands_db(File, M:Commands)).

select_option(Holder-Default, OptionL0, OptionL) :-
    select_option(Holder, OptionL0, OptionL, Default).

collect_expansion_commands(goal_cw, Term, Into, Expander, OptionL0,
			   FileCommands) :-
    !,
    option_allchk(OptionL0, OptionL1, AllChk),
    maplist_dcg(select_option, [module(M)     -M,
				caller(Caller)-Caller],
		OptionL1, OptionL2),
    (nonvar(M)-> OptionL3 = [module(M)|OptionL2] ; OptionL3 = OptionL2),
    prolog_walk_code(
	[ trace_reference(M:Term),
	  infer_meta_predicates(false),
	  evaluate(false),
	  on_trace(collect_file_commands(Caller, Term, Into, Expander, AllChk))
	| OptionL3
	]),
    findall(File-Commands, retract(file_commands_db(File, Commands)), FileCommands).
collect_expansion_commands(Level, Term, Into, Expander, Options0, FileCommands) :-
    level_hook(Level, Options0, Options, Expanded,
	       collect_ec_term_level(Level, Expanded, Term, Into, Expander,
				     Options, FileCommands)).

expand_holder(yes, ex(_)).
expand_holder(no,  no).

collect_ec_term_level(Level, Expanded, Term, Into, Expander,
		      OptionL, FileCommands) :-
    expand_holder(Expanded, ExHolder),
    findall(File-Commands,
	    ec_term_level_each(Level, ExHolder, Term, Into,
			       Expander, File, Commands, OptionL),
	    FileCommands).

:- public mod_prop/2.
mod_prop([],   Module) :- !, current_module(Module).
mod_prop(Prop, Module) :- module_property(Module, Prop).

ec_term_level_each(Level, ExHolder, Term, Into,
		   Expander, File, M:Commands, OptionL0) :-
    option_allchk(OptionL0, OptionL1, AllChk),
    maplist_dcg(select_option, [module_property(Prop)-[],
				syntax_errors(SE)-error,
				subterm_positions(TermPos)-TermPos,
				module(M)-M,
				sentence(SentPattern)-SentPattern
			       ],
		OptionL1, OptionL2),
    OptionL = [syntax_errors(SE),
	       subterm_positions(TermPos),
	       module(M)|OptionL2],
    mod_prop(Prop, M),
    (Level = sent -> SentPattern = Term ; true), % speed up
    with_context_vars(( get_term_info(M, SentPattern, Sent, ExHolder,
				      AllChk, File, OptionL),
			phrase(substitute_term_level(Level, Sent, 1200, Term,
						     Into, Expander, TermPos),
			       Commands, [])
		      ),
		      [refactor_sent_pattern,
		       refactor_sentence,
		       refactor_expanded,
		       refactor_options,
		       refactor_goal_args],
		      [SentPattern,
		       Sent,
		       ExHolder,
		       OptionL,
		       ga(Term, Into, Expander)]).

substitute_term_level(goal, _, _, _, _, _, _) -->
    findall(Commands, retract(command_db(Commands))).
substitute_term_level(term, Sent, Priority, Term, Into, Expander, TermPos) -->
    substitute_term_rec(Sent, Priority, Term, Into, Expander, TermPos).
substitute_term_level(sent, Sent, Priority, Term, Into, Expander, TermPos) -->
    substitute_term_norec(top, Sent, Priority, Term, Into, Expander, TermPos).
substitute_term_level(head, Clause, Priority, Term, Into, Expander, TermPos) -->
    substitute_term_head(norec, Clause, Priority, Term, Into, Expander, TermPos).
substitute_term_level(head_rec, Clause, Priority, Term, Into, Expander, TermPos) -->
    substitute_term_head(rec, Clause, Priority, Term, Into, Expander, TermPos).
substitute_term_level(body, Clause, _, Term, Into, Expander, TermPos) -->
    substitute_term_body(norec, Clause, Term, Into, Expander, TermPos).
substitute_term_level(body_rec, Clause, _, Term, Into, Expander, TermPos) -->
    substitute_term_body(rec, Clause, Term, Into, Expander, TermPos).

substitute_term_body(Rec, (_ :- Body), Term, Into, Expander,
		     term_position(_, _, _, _, [_, BodyPos])) -->
    {term_priority((_ :- Body), 2, Priority)},
    substitute_term(Rec, Body, Priority, Term, Into, Expander, BodyPos).

substitute_term_head(Rec, Clause, Priority, Term, Into, Expander, TermPos) -->
    { Clause = (Term :- _)
    ->term_priority(Clause, 1, HPriority),
      term_position(_, _, _, _, [HeadPos, _]) = TermPos
    ; Term = Clause,
      HPriority = Priority,
      HeadPos = TermPos
    },
    substitute_term(Rec, Term, HPriority, Term, Into, Expander, HeadPos).

substitute_term(rec, Term, Priority, Pattern, Into, Expander, TermPos) -->
    substitute_term_rec(Term, Priority, Pattern, Into, Expander, TermPos).
substitute_term(norec, Term, Priority, Pattern, Into, Expander, TermPos) -->
    substitute_term_norec(top, Term, Priority, Pattern, Into, Expander, TermPos).

:- meta_predicate with_pattern_into(0, ?, ?).
with_pattern_into(Goal, Pattern, Into) :-
    with_context_vars(Goal, [refactor_pattern, refactor_into], [Pattern, Into]).

:- meta_predicate with_from(0, ?).
with_from(Goal, From) :-
    with_context_vars(Goal, [refactor_from], [From]).

:- meta_predicate with_context_vars(0, +, +).
with_context_vars(Goal, NameL, ValueL) :-
    setup_call_cleanup(maplist(b_setval, NameL, ValueL),
		       Goal,
		       maplist(nb_delete, NameL)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ANCILLARY PREDICATES:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apply_file_commands(Pairs, FileContent) :-
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    findall(Compact, ( member(Group, Grouped),
		       compact_group(Group, Compact)),
	    Compacted),
    maplist(apply_commands, Compacted, FileContent).

cg1(M:L, ML) :-
    maplist(cg2(M), L, ML).

cg2(M, E, M:E).

compact_group(Key-MLList, Key-List) :-
    maplist(cg1, MLList, LList),
    append(LList, UList),
    UList \== [],
    sort(UList, List).

apply_commands(File-Commands, File-NewText) :-
    ( pending_change(_, File, Text) -> true
    ; read_file_to_string(File, Text, [])
    ),
    length(Commands, N),
    print_message(informational, format('~w changes in ~w', [N, File])),
    with_context_vars(maplist_dcg(apply_change,
				  Commands,
				  text_desc(0, Text, NewTextL),
				  text_desc(_, Remaining, [Remaining])),
		      [refactor_text, refactor_file],
		      [Text, File]),
    maplist_dcg(string_concat_to, NewTextL, "", NewText).

string_concat_to(A, B, C) :- string_concat(B, A, C).

:- meta_predicate with_context(?, ?, ?, ?, ?, 0).
with_context(Src, Pattern0, Into0, Pattern1, Into2, Goal) :-
    copy_term(Pattern0-Into0, Pattern1-Into1),
    refactor_context(sentence, Sent),
    refactor_context(sent_pattern, Sent),
    Pattern0 = Src,
    with_pattern_into(once(Goal), Pattern1, Into1), % Allow changes in Pattern/Into
    term_variables(Pattern1, Vars), % Variable bindings in Pattern
    %% Apply changes to Pattern/Into and bind Vars:
    copy_term(t(Pattern1, Into1, Vars), t(Pattern0, Into0, Vars0)),
    pairs_keys_values(Pairs, Vars0, Vars),
    map_subterms(Pairs, Into0, Into1, Into2).

map_subterms(Pairs, T0, T1, T) :-
    ( member(X0-X, Pairs),
      X==T1
    ; member(X0-X, Pairs),
      same_term(X0, T0) % ===/2
    ),
    !,
    (T0==T1 -> T = X0 ; T = X).
map_subterms(Pairs, T0, T1, T) :-
    compound(T0), !,
    map_compound(Pairs, T0, T1, T).
map_subterms(_, T, _, T).

map_compound(Pairs,		% Special case: preserve Goal
	     '$G'(T0, G),
	     '$G'(T1, _),
	     '$G'(T,  G)) :- !,
    map_subterms(Pairs, T0, T1, T).
map_compound(Pairs, T0, T1, T) :-
    functor(T0, F, N),
    functor(T1, F, N),
    functor(T,  F, N),
    T0 =.. [F|Args0],
    T1 =.. [F|Args1],
    T  =.. [F|Args],
    maplist(map_subterms(Pairs), Args0, Args1, Args).

%%	substitute_term_norec(+Term, +Priority, +Pattern, -Into, :Expander, +TermPos)// is nondet.
%
%	None-recursive version of substitute_term_rec//6.

special_term(sub, Term, Term).
special_term(top, Term0, Term) :- top_term(Term0, Term).

top_term(Var, Var) :- var(Var), !.
top_term(List, '$LIST.NL'(List)) :- List = [_|_], !.
top_term([], '$RM') :- !.
top_term(Term, Term).

substitute_term_norec(Sub, Term, Priority, Pattern, Into, Expander, TermPos) -->
    { refactor_context(sentence,     Sent),
      refactor_context(sent_pattern, SentPattern),
      subsumes_term(SentPattern-Pattern, Sent-Term),
      copy_term(Term, Term2),
      with_context(Term, Pattern, Into, Pattern1, Into1, Expander),
      greatest_common_binding(Pattern1, Into1, Pattern2, Into2, [[]], Unifier, []),
      special_term(Sub, Into2, Into3)
    },
    perform_substitution(Priority, Term, Term2, Pattern2, Into3, Unifier, TermPos).

%%	perform_substitution(+Priority, +SrcTerm, +Pattern, +Into, +Unifier, +TermPos)
%
%	Substitute occurences of Pattern with Into after calling
%	expansion.
%
%	@param SrcTerm is the term as read from the source
%	@param TermPos is the term layout of SrcTerm
%	@param Priority is the environment operator priority
%	@param Unifier contains bindings between Pattern and Into

perform_substitution(Priority, Term, Term2, Pattern2, Into2, BindingL, TermPos) -->
    { copy_term(Term2, GTerm),
      unifier(Term2, Term, UL0),
      maplist_dcg(non_singleton(UL0), UL0, UL1, []),
      maplist_dcg(substitute_2, UL1, sub(Term2, UL3), sub(Term3, [])),
      % UL1=UL2, Term2=Term3, UL3=[],
      UL1=UL2,
      with_context_vars(subst_term(TermPos, Pattern2, GTerm, Priority, Term3),
			[refactor_bind], [BindingL]),
      maplist(subst_unif(Term3, TermPos, GTerm), UL3),
      maplist(subst_unif(Term3, TermPos, GTerm), UL2),
      maplist(subst_fvar(Term2, TermPos, GTerm), UL0)
    },
    !,
    [subst(TermPos, Priority, Pattern2, GTerm, Into2)].

non_singleton(L, V1=V2) -->
    ( { var(V1),
	var(V2),
	( occurrences_of_var(V2, L, 1)
	; occurrences_of_var(V1, L, 1)
	)
      }
    ->{V1=V2}
    ; [V1=V2]
    ).

unifier(Term1, Term2, L) :-
    term_variables(Term1, Var1),
    copy_term(Term1-Var1, Term2-Var2),
    maplist(eq, Var1, Var2, L).

eq(A, B, A=B).

get_position_gterm(Term, Pos, GTerm, T, GPos, G, GPriority) :-
    subterm_location_eq(L, T, Term),
    subpos_location(L, Pos, GPos),
    ( append(L0, [E], L) ->
      subterm_location(L0, GP, GTerm),
      subterm_location([E], G, GP),
      term_priority(GP, E, GPriority)
    ; GPriority = 999,
      subterm_location(L, G, GTerm)
    ).

substitute_2(V0=T0, sub(Term0, BL0), sub(Term, BL)) :-
    substitute_value(T0, V1, Term0, Term1),
    ( Term0 == Term1
    ->greatest_common_binding(Term0, T0, Term, T, [[]], BL0, BL1),
      ( BL1==BL0
      ->BL = BL1
      ; BL1 = [V0=T|BL]
      )
    ; Term = Term1,
      BL0 = [V0=V1|BL]
    ).

subst_unif(Term, Pos, GTerm, V=T) :-
    ( ( (var(T) ; nonvar(T), T \= '$sb'(_, _, _, _)),
	get_position_gterm(Term, Pos, GTerm, T, GPos, G, P),
	GPos \= none
      ->ignore(V='$sb'(GPos, G, P, T))
      ; get_position_gterm(Term, Pos, GTerm, V, GPos, G, P),
	GPos \= none
      ->ignore(V='$sb'(GPos, G, P, T))
      )
    ; V=T
    ).

subst_fvar(Term, Pos, GTerm, V=T) :-
    ( var(V),
      V==T,
      get_position_gterm(Term, Pos, GTerm, V, GPos, G, _P) ->
      V='$sb'(GPos, G)
    ; true
    ).

subst_args(N, Term, GTerm, CTerm, [ArgPos|SubPos]) :-
    arg(N, Term,  Arg),
    !,
    arg(N, GTerm, GArg),
    arg(N, CTerm, CArg),
    term_priority(GTerm, N, GPriority),
    subst_term(ArgPos, Arg, GArg, GPriority, CArg),
    succ(N, N1),
    subst_args(N1, Term, GTerm, CTerm, SubPos).
subst_args(_, _, _, _, _).

subst_list([], _, Tail, E, G, C) :-
    term_priority([_|_], 2, P),
    subst_term(Tail, E, G, P, C).
subst_list([Pos|Poss], To, Tail, [E|Es], [G|Gs], [C|Cs]) :-
    term_priority([_|_], 1, P),
    subst_term(Pos, E, G, P, C),
    subst_list(Poss, To, Tail, Es, Gs, Cs).

subst_var(Pos, Var, GTerm, GPriority, CTerm) :-
    ( b_getval(refactor_bind, BindingL),
      member(V=T, BindingL),
      V==Var
    ->subst_term(Pos, T, GTerm, GPriority, CTerm)
    ; true
    ),
    Var = '$sb'(Pos, GTerm, GPriority, CTerm).

%%	subst_term(+Position, +Pattern, +Vars, +Term)
%
%	Here, Pattern is a term  that   holds  variables.  It is matched
%	against a position term and  if  there   is  a  variable  in the
%	pattern, this is unified to   a  '$sb'(Pos, SubTerm),
%	indicating that this position currently holds SubTerm.
%
%	@param Position is a subterm-position term for Term
%	@param Term is a source term
%	@param Pattern is a substitution pattern

subst_term(none, T, _, _, T) :- !.
subst_term(Pos, Term, GTerm, GPriority, CTerm) :-
    var(Term),
    !,
    subst_var(Pos, Term, GTerm, GPriority, CTerm).
subst_term(term_position(_, _, _, _, CP), Term, GTerm, _, CTerm) :-
    compound(CTerm), % Would have been substituted
    !,
    subst_args(1, Term, GTerm, CTerm, CP).
subst_term(brace_term_position(_, _, CP), {Term}, {GTerm}, _, {CTerm}) :- !,
    subst_term(CP, Term, GTerm, 999, CTerm).
subst_term(list_position(_, To, Elms, Tail), Term, GTerm, _, CTerm) :- !,
    subst_list(Elms, To, Tail, Term, GTerm, CTerm).
subst_term(_, _, _, _, _).

%%	substitute_term_rec(+SrcTerm, +Priority, +Pattern, -Into, :Expander, +TermPos)// is nondet.
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

substitute_term_rec(Term, Priority, Pattern, Into, Expander, TermPos) -->
    substitute_term_norec(sub, Term, Priority, Pattern, Into, Expander, TermPos),
    !.
substitute_term_rec(Term, _, Ref, Into, Expander, TermPos) -->
    substitute_term_into(TermPos, Term, Ref, Into, Expander).

substitute_term_into(brace_term_position(_, _, Pos), {Term},
		     Ref, Into, Expander) -->
    substitute_term_rec(Term, 1200, Ref, Into, Expander, Pos).
substitute_term_into(term_position(_, _, _, _, CP), Term, Ref,
		     Into, Expander) --> !,
    substitute_term_args(CP, 1, Term, Ref, Into, Expander).
substitute_term_into(list_position(_, _, EP, TP), Term, Ref,
		     Into, Expander) --> !,
    substitute_term_list(EP, TP, Term, Ref, Into, Expander).

:- use_module(library(listing), []).

term_priority(Term, N, Priority) :-
    nonvar(Term),
    term_priority_gnd(Term, N, Priority).

% term_priority_gnd('$sb'(_, _, _, Term), N, Priority) :- !,
%     term_priority(Term, N, Priority).
term_priority_gnd(Term, N, Priority) :-
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
    ; Priority=999 % term_priority((_, _), 1, Priority)
    ).

substitute_term_args([PA|PAs], N0, Term, Ref, Into, Expander) -->
    ( {arg(N0, Term, Arg)},
      {term_priority(Term, N0, Priority)},
      substitute_term_rec(Arg, Priority, Ref, Into, Expander, PA)
    ; {N is N0 + 1},
      substitute_term_args(PAs, N, Term, Ref, Into, Expander)
    ).

substitute_term_list([EP|EPs], TP, [Elem|Term], Ref, Into, Expander) -->
    ( {term_priority([_|_], 1, Priority)},
      substitute_term_rec(Elem, Priority, Ref, Into, Expander, EP)
    ; substitute_term_list(EPs, TP, Term, Ref, Into, Expander)
    ).
substitute_term_list([], TP, Tail, Ref, Into, Expander) -->
    {term_priority([_|_], 2, Priority)},
    substitute_term_rec(Tail, Priority, Ref, Into, Expander, TP).

:- multifile
    prolog:message//1,
    prolog:message_location//1.

prolog:message(acheck(refactor(Goal, From))) -->
    prolog:message_location(From),
    ['Unable to refactor ~w, no term position information available'-[Goal], nl].

compound_positions(Line1, Pos1, Pos0, Pos) :-
    Line1 =< 1,
    !,
    Pos is Pos0 + Pos1.
compound_positions(_, Pos, _, Pos).

% :- use_module(library(prolog_codewalk), []). % For filepos_line/4

textpos_line(Text, CharPos, Line, LinePos) :-
    setup_call_cleanup(
	( open_codes_stream(Text, In),
	  open_null_stream(Out)
	),
	( copy_stream_data(In, Out, CharPos),
	  stream_property(In, position(Pos)),
	  stream_position_data(line_count, Pos, Line),
	  stream_position_data(line_position, Pos, LinePos)
	),
	( close(Out),
	  close(In)
	)).

get_output_position(Pos) :-
    b_getval(refactor_from, From),
    b_getval(refactor_text, Text),
    textpos_line(Text, From, _Line0, Pos0),
    stream_property(current_output, position(StrPos)),
    stream_position_data(line_count, StrPos, Line1),
    stream_position_data(line_position, StrPos, Pos1),
    compound_positions(Line1, Pos1, Pos0, Pos).

rportray_body(B, Offs, OptL) :-
    get_output_position(Pos),
    write_b(B, OptL, Offs, Pos).

rportray_clause(C, Offs, OptL) :-
    ( nonvar(C),
      C = (H :- B)
    ->write_term(H, OptL),
      write(' :-\n'),
      get_output_position(Pos),
      LinePos is Offs + Pos,
      line_pos(LinePos),
      write_b(B, OptL, Offs, Pos)
    ; write_term(C, OptL)
    ).

:- dynamic rportray_skip/0.

:- public rportray/2.
rportray('$sb'(TermPos, _), _Opt) :-
    !,
    \+ retract(rportray_skip),
    b_getval(refactor_text, Text),
    print_subtext(TermPos, Text).
rportray('$sb'(ArgPos, GTerm, GPriority, Term), OptionL) :-
    \+ retract(rportray_skip),
    !,
    ignore((b_getval(refactor_text, Text),
	    print_expansion_sb(Term, Term, GTerm, ArgPos, GPriority, OptionL, Text)
	   )).
% rportray('$sb'(_, _, _, _), _) :- !.
rportray('$@'(Term, '$sb'(ArgPos, GTerm, GPriority, Pattern)), OptionL) :-
    !,
    %% Use a different pattern to guide printing of Term
    b_getval(refactor_text, Text),
    print_expansion_sb(Term, Pattern, GTerm, ArgPos, GPriority, OptionL, Text),
    !.
rportray('$G'(Term, Goal), Opt) :-
    !,
    with_str_hook(write_term(Term, Opt), Goal).
rportray('$NOOP'(Term), Opt) :- !,
    with_output_to(string(_),	%Ignore, but process
		   write_term(Term, Opt)).
rportray('$LIST'(L), Opt) :- !,
    maplist(term_write(Opt), L).
rportray('$LIST,'(L), Opt) :- !,
    term_write_comma_list(L, Opt).
rportray('$LIST,_'(L), Opt) :- !,
    maplist(term_write_comma_2(Opt), L).
rportray('$TEXT'(T), Opt) :- !,
    write_t(T, Opt).
rportray('$TEXT'(T,_), Opt) :- !,
    write_t(T, Opt).
rportray('$CLAUSE'(C), Opt) :- !,
    rportray_clause(C, 4, Opt).
rportray('$CLAUSE'(C, Offs), Opt) :- !,
    rportray_clause(C, Offs, Opt).
rportray('$BODY'(B, Offs), Opt) :- !,
    rportray_body(B, Offs, Opt).
rportray('$BODY'(B), Opt) :- !,
    rportray_body(B, 0, Opt).
rportray([E|T0], Opt) :- !,
    append(H, T1, [E|T0]),
    ( var(T1) -> !,
      fail
    ; T1 = '$sb'(TermPos, GTerm, GPriority, Term),
      is_list(Term),
      compound(TermPos),
      !,
      arg(1, TermPos, TFrom),
      arg(2, TermPos, TTo),
      succ(TFrom, From),
      succ(To, TTo),
      T2 = '$sb'(From-To, GTerm, GPriority, Term),
      ( Term == []
      ->T = H,
	write('['),
	term_write_comma_list(T, Opt),
	write_term(T2, Opt),
	write(']')
      ; append(H, [T2], T),
	write_term(T, Opt)
      )
    ).

term_write(Opt, Term) :- write_term(Term, Opt).

term_write_comma_list([], _).
term_write_comma_list([T|L], Opt) :-
    write_term(T, Opt),
    maplist(term_write_comma_(Opt), L).

term_write_comma_(Opt, Term) :- write(', '), write_term(Term, Opt).

term_write_comma_2(Opt, Term) :- write_term(Term, Opt), write(', ').

:- use_module(library(listing), []).

cut_text(Pos0, Pos, Remaining0, Remaining, Text) :-
    ( Pos > Pos0 ->
      Seek is Pos - Pos0,
      sub_string(Remaining0, 0, Seek, _, Text),
      sub_string(Remaining0, Seek, _, 0, Remaining)
    ; Remaining0 = Remaining,
      Text = ""
    ).

apply_change(M:subst(TermPos, Priority, Pattern, Term, Into),
	     text_desc(Pos, Text0, [CutText, PasteText|Tail]),
	     text_desc(To,  Text,  Tail)) :-
    b_getval(refactor_text, RText),
    wr_options(OptionL0),
    OptionL = [module(M), priority(Priority)|OptionL0 ],
    with_output_to(string(PasteText),
		   print_expansion_0(Into, Pattern, Term, TermPos, OptionL, RText, From, To)),
    cut_text(Pos, From, Text0, Text1, CutText),
    cut_text(From, To, Text1, Text, _). % Skip

print_expansion_0(Into, Pattern, Term, TermPos, OptionL, Text, From, To) :-
    ( nonvar(Into) ->
      print_expansion_1(Into, Pattern, Term, TermPos, OptionL, Text, From, To)
    ; print_expansion_2(Into, Pattern, Term, TermPos, OptionL, Text, From, To)
    ).

wr_options([portray_goal(ref_expand:rportray),
	    spacing(next_argument),
	    numbervars(true),
	    quoted(true),
	    partial(true)]).

print_expansion_rm_dot(TermPos, Text, From, To) :-
    arg(1, TermPos, From),
    arg(2, TermPos, Before),
    sub_string(Text, Before, _, 0, Right),
    sub_string(Right, Next, _, _, "."),
    To is Before + Next + 2.

/*
% Hacks that can only work at 1st level:
% BUG: assuming no spaces between Term, full stop and new line.
% The following predicate would give a hint about how to implement
% the correct dot position:

:- use_module(library(prolog_source)).
clause_line_interval(Clause, LineI) :-
    clause_property(Clause, line_count(Line1)),
    ( clause_property(Clause, file(File)),
      module_property(Module, file(File)),
      catch(open(File, read, In), _, fail),
      set_stream(In, newline(detect)),
      call_cleanup(( read_source_term_at_location(In, _Term,
						  [ line(Line1),
						    module(Module)
						  ]),
		     stream_property(In, position(Pos))
		   ),
		   close(In)),
      stream_position_data(line_count, Pos, Line20 ),
      succ(Line2, Line20 ),	% one line back
      Line1 \= Line2
    ->LineI = Line1-Line2
    ; LineI = Line1
    ).
*/
print_expansion_1('$RM', _, _, TermPos, _, Text, From, To) :- !,
    print_expansion_rm_dot(TermPos, Text, From, To).
print_expansion_1('$TEXT'(Term), _, _, TermPos, OptionL, _, From, To) :- !,
    arg(1, TermPos, From),
    arg(2, TermPos, To),
    % quoted(false)
    write_t(Term, OptionL).
print_expansion_1('$TEXT'(Term, Delta), _, _, TermPos, OptionL, _, From, To) :- !,
    arg(1, TermPos, From),
    arg(2, TermPos, To0),
    % quoted(false)
    write_t(Term, OptionL),
    To is To0 + Delta.
print_expansion_1('$LIST.NL'(TermL), _, _, TermPos, OptionL0, Text, From, To) :- !,
    merge_options([priority(1200)], OptionL0, OptionL),
    maplist(term_write_stop_nl(OptionL), TermL),
    print_expansion_rm_dot(TermPos, Text, From, To).
print_expansion_1(Into, Pattern, Term, TermPos, OptionL, Text, From, To) :-
    print_expansion_2(Into, Pattern, Term, TermPos, OptionL, Text, From, To).

term_write_stop_nl(Opt, Term) :-
    term_write_stop_nl__(Term, Opt).

term_write_stop_nl__('$NL', _) :- !, nl.
term_write_stop_nl__('$NOOP'(Term), Opt) :-
    with_output_to(string(_),	%Ignore, but process
		   term_write_stop_nl__(Term, Opt)).
term_write_stop_nl__(Term, Opt) :-
    write_term(Term, Opt),
    write('.'),nl.

print_expansion_2(Into, Pattern, Term, TermPos, OptionL, Text, From, To) :-
    arg(1, TermPos, From),
    arg(2, TermPos, To),
    with_from(print_expansion(Into, Pattern, Term, TermPos, OptionL, Text), From).


% if the term have been in parentheses, in a place where that was
% required, include it!!!
%
fix_position_if_braced(term_position(From0, To0, FFrom, FTo, PosL),
		       term_position(From,  To,  FFrom, FTo, PosL),
		       Term, GPriority, Into, Priority, Text, Display) :-
    \+ ( From0 == FFrom,
	 sub_string(Text, FTo, 1, _, "(")
       ),
    ( prolog_listing:term_needs_braces(Term, GPriority),
      ( nonvar(Into),
	prolog_listing:term_needs_braces(Into, Priority)
      ; prolog_listing:term_needs_braces(Term, Priority)
      )
    ->once(( between(1, From0, LCount),
	     From is From0 - LCount,
	     sub_string(Text, From, 1, _, "(")
	   )),
      once(( string_length(Text, Length),
	     between(To0, Length, To1),
	     sub_string(Text, To1, 1, _, ")"),
	     succ(To1, To)
	   )),
      Display=no
    ; \+ ( prolog_listing:term_needs_braces(Term, GPriority),
	   \+ ( From0 == FFrom,
		sub_string(Text, FTo, 1, _, "(")
	      )
	 ),
      ( nonvar(Into),
	prolog_listing:term_needs_braces(Into, Priority)
      ; prolog_listing:term_needs_braces(Term, Priority)
      )
    ->Display=yes,
      From = From0,
      To = To0
    ),
    !.
fix_position_if_braced(Pos, Pos, _, _, _, _, _, no). % fail-safe

comp_priority(GTerm, GPriority, Term, Priority) :-
    \+prolog_listing:term_needs_braces(GTerm, GPriority),
    prolog_listing:term_needs_braces(Term, Priority).

cond_display(yes, A) :- display(A).
cond_display(no,  _).

%% print_expansion(?Term:term, N:integer, File:atom, Pos0:integer, SkipTo:integer).
%
print_expansion_sb(Into, Pattern, Term, RefPos, GPriority, OptionL, Text) :-
    select_option(priority(Priority), OptionL, _, Priority),
    fix_position_if_braced(RefPos, TermPos, Term, GPriority, Into, Priority, Text, Display),
    cond_display(Display, '('),
    arg(1, TermPos, From),
    with_from(print_expansion_ne(Into, Pattern, Term, TermPos, OptionL, Text), From),
    cond_display(Display, ')').

% TODO: stream position would be biased --EMM
with_str_hook(Command, StrHook) :-
    with_output_to(string(S0), call(Command)),
    ( call(StrHook, S0, S)
    ->true
    ; S = S0
    ),
    format('~s', [S]).

print_expansion(Var, _, _, RefPos, _, Text) :-
    var(Var),
    !,
    print_subtext(RefPos, Text).
print_expansion('$sb'(RefPos, _), _, Term, _, _, Text) :-
    \+ ( nonvar(Term),
	 Term = '$sb'(_, _)
       ),
    !,
    print_subtext(RefPos, Text).
print_expansion('$sb'(RefPos, Term1, Priority, Into), _, Term, _, OptionL, Text) :-
    \+ ( nonvar(Term),
	 Term = '$sb'(_, _, _, _),
	 Into \= '$sb'(_, _, _, _)
       ),
    !,
    print_expansion_sb(Into, Into, Term1, RefPos, Priority, OptionL, Text).
print_expansion(Into, Pattern, Term, RefPos, OptionL, Text) :-
    print_expansion_ne(Into, Pattern, Term, RefPos, OptionL, Text).

print_expansion_ne(Var, _, _, RefPos, _, Text) :-
    var(Var),
    !,
    print_subtext(RefPos, Text).
print_expansion_ne('$G'(Into, Goal), Pattern, Term, RefPos, OptionL, Text) :-
    \+ ( nonvar(Term),
	 Term = '$G'(_, _)
       ),
    !,
    with_str_hook(print_expansion(Into, Pattern, Term, RefPos, OptionL, Text),
		  Goal).
print_expansion_ne('$,NL', Pattern, Term, RefPos, OptionL, Text) :-
    Term \=='$,NL',
    !,
    %% Print a comma + indented new line
    write(','),
    print_expansion('$NL', Pattern, Term, RefPos, OptionL, Text).
print_expansion_ne('$NL', _, Term, _, _, Text) :- % Print an indented new line
    Term \== '$NL',
    !,
    b_getval(refactor_from, From),
    textpos_line(Text, From, _, LinePos),
    nl,
    line_pos(LinePos).
/* In quarintine, seems to be that now is useless due to the gcb.pl --EMM
print_expansion_ne(Into, Pattern, Term0, RefPos0, OptionL, Text) :-
    compound(Into),
    Into \== Pattern,
    subterm_location_eq(L, Into, Pattern),
    subterm_location(   L, Term, Term0),
    subpos_location(L, RefPos0, RefPos),
    !,
    print_expansion_ne(Into, Into, Term, RefPos, OptionL, Text).
*/
print_expansion_ne(Into, SPattern, Term1, _, OptionL, Text) :-
    nonvar(SPattern),
    nonvar(Term1),
    Term1\='$sb'(_, _, _, _),	% is not a readed term, but a command
    SPattern='$sb'(RefPos, Term, _, Pattern),
    !,
    print_expansion_ne(Into, Pattern, Term, RefPos, OptionL, Text).
    % print_expansion_sb(RefPos, GTerm, GPriority, Term, Pattern, OptionL, Text).
print_expansion_ne(Into, Pattern, Term, RefPos, OptionL, Text) :-
    ( print_expansion_pos(RefPos, Into, Pattern, Term, OptionL, Text)
    ->true
    ; write_term(Into, OptionL)
    ).

print_expansion_arg(MTerm, OptionL0, Text, N, FromTo, RefPos, Term, Pattern, GTerm) :-
    term_priority(MTerm, N, Priority),
    merge_options([priority(Priority)], OptionL0, OptionL),
    print_expansion_elem(OptionL, Text, FromTo, RefPos, Term, Pattern-GTerm).

print_expansion_elem(OptionL, Text, From-To, RefPos, Term, Pattern-GTerm) :-
    print_expansion(Term, Pattern, GTerm, RefPos, OptionL, Text),
    display_subtext(Text, From, To).

% valid_op_type_arity(xf,  1).
% valid_op_type_arity(yf,  1).
% valid_op_type_arity(xfx, 2).
% valid_op_type_arity(xfy, 2).
% valid_op_type_arity(yfx, 2).

% valid_op_type_arity(fy,  1).
% valid_op_type_arity(fx,  1).

from_to_pairs([], _, To, To) --> [].
from_to_pairs([Pos|PosL], From0, To0, To) -->
    { arg(1, Pos, To1),
      (To1 = 0 -> To0 = From0 ; To0 = To1),
      arg(2, Pos, From1),
      (From1 = 0 -> From = To0 ; From = From1)
    },
    [From-To2],
    from_to_pairs(PosL, From, To2, To).
print_expansion_pos(term_position(From, To, _FFrom, FFTo, PosL), Into, Pattern,
		    GTerm, OptionL, Text) :-
    compound(Into),
    functor(Into,    FT, A),
    functor(Pattern, FP, A),
    from_to_pairs(PosL, FFTo, To1, To, FromToL, []),
    FromToT =.. [FT|FromToL],
    PosT    =.. [FP|PosL],
    !,
    ( FT == FP
    ->display_subtext(Text, From, To1) %% Do nothing to preserve functor layout
    % TBD: the commented out lines must be re-tested, because they have problems
    % if the operator is not prefix (e.g., a+b ---> a/b)
    % TBD: use '$exported_op'/3
    ; select_option(module(M), OptionL, _, M),
      \+ M:current_op(_, _, FT),
      \+ M:current_op(_, _, FP)
      % ( valid_op_type_arity(TypeOp, A),
      % 	current_op(PrecedenceP, TypeOp, FT),
      % 	current_op(PrecedenceE, TypeOp, FP)
      % ->PrecedenceP >= PrecedenceE
      % ; valid_op_type_arity(TypeOp, A),
      % 	\+ current_op(_, TypeOp, FT),
      % 	\+ current_op(_, TypeOp, FP)
      % ->true
      % ; \+ valid_op_type_arity(_, A)
      % )
    ->merge_options([priority(999)], OptionL, OptionL1),
      write_term(FT, OptionL1),
      ( FFTo > To1 -> true % TODO: try to understand why this happens --EMM
      ; display_subtext(Text, FFTo, To1)
      )
    ),
    mapargs(print_expansion_arg(Into, OptionL, Text), FromToT, PosT, Into, Pattern, GTerm).
print_expansion_pos(list_position(From, To, PosL, PosT), Into, Pattern, GTerm, OptionL0, Text) :-
    from_to_pairs(PosL, From, To1, To2, FromToL, []),
    length(PosL, N),
    ( trim_list(N, Into, ArgL, ATail)
    ->Delta = 0
    ; length(LTerm, N), % Layout preserved if list is converted to sequence
      once(list_sequence(LTerm, Into)),
      trim_list(N, LTerm,    ArgL, ATail),
      Delta = 1
    ),
    \+ ( PosT = none,
	 ATail \= []
       ),
    trim_list(N, Pattern, PatL, PTail),
    trim_list(N, GTerm,   GTrL, GTail),
    pairs_keys_values(PatGTrL, PatL, GTrL),
    !,
    From1 is From + Delta,
    term_priority(Into, 1, Priority1),
    select_option(priority(Priority), OptionL0, OptionL, Priority),
    OptionL1=[priority(Priority1)|OptionL],
    ( comp_priority(GTerm, Priority, Into, Priority)
    ->display('(')
    ; Delta = 1 ->display(' ')	% Only if [...] ---> (...)
    ; true
    ),
    display_subtext(Text, From1, To1),
    ( PosT \= none ->
      arg(1, PosT, PTo),
      term_priority(Into, 2, Priority2),
      To2 is PTo + Delta,
      maplist(print_expansion_elem(OptionL1, Text), FromToL, PosL, ArgL, PatGTrL),
      arg(2, PosT, PFrom),
      OptionL2=[priority(Priority2)|OptionL],
      print_expansion_elem(OptionL2, Text, PFrom-To, PosT, ATail, PTail-GTail)
    ; To2 is To - Delta,
      maplist(print_expansion_elem(OptionL1, Text), FromToL, PosL, ArgL, PatGTrL)
    ),
    (comp_priority(GTerm, Priority, Into, Priority) ->display(')') ; true).
print_expansion_pos(brace_term_position(From, To, TermPos), {Into}, {Pattern},
		    {GTerm}, OptionL, Text) :-
    arg(1, TermPos, AFrom),
    arg(2, TermPos, ATo),
    display_subtext(Text, From, AFrom),
    print_expansion_arg({Into}, OptionL, Text, 1, ATo-To, TermPos, Into, Pattern, GTerm).
print_expansion_pos(From-To, Into, _Pattern, GTerm, _, Text) :-
    Into==GTerm,
    display_subtext(Text, From, To).

print_subtext(RefPos, Text) :-
    arg(1, RefPos, From),
    arg(2, RefPos, To),
    display_subtext(Text, From, To).

trim_list(N, L0, L, T) :-
    length(L, N),
    append(L, T, L0).

subterm_location_eq([],    Find, Term) :- Find==Term.
subterm_location_eq([N|L], Find, Term) :-
    compound(Term),
    arg(N, Term, SubTerm),
    subterm_location_eq(L, Find, SubTerm).

subterm_location([],    Term, Term).
subterm_location([N|L], Find, Term) :-
    compound(Term),
    arg(N, Term, SubTerm),
    subterm_location(L, Find, SubTerm).

location_subpos(term_position(_, _, _, _, PosL), N, Pos) :-
    nth1(N, PosL, Pos).
location_subpos(list_position(From, To, PosL, Tail), N, Pos) :-
    ( N = 1
    ->PosL = [Pos|_]
    ; N = 2
    ->( PosL = [_]
      ->Pos = Tail
      ; PosL = [_|PosL1],
	Pos = list_position(From, To, PosL1, Tail)
      )
    ).
location_subpos(brace_term_position(_, _, Pos), 1, Pos).

subpos_location([],    Pos,    Pos).
subpos_location([N|L], SubPos, Pos) :-
    location_subpos(SubPos, N, Pos0),
    subpos_location(L, Pos0, Pos).

display_subtext(Text0, From, To) :-
    ( From == To
    ->true
    ; get_subtext(Text0, From, To, Text),
      format('~s', [Text])
    ).

get_subtext(Text0, From, To, Text) :-
    LPaste is To - From,
    sub_string(Text0, From, LPaste, _, Text).

bin_op(Term, Op, Left, Right, A, B) :-
    nonvar(Term),
    functor(Term, Op, N),
    N == 2,
    prolog_listing:infix_op(Op, Left, Right),
    arg(1, Term, A),
    arg(2, Term, B).

write_b(Term, OptL, Offs, Pos0) :-
    ( memberchk(priority(N), OptL),
      prolog_listing:term_needs_braces(Term, N)
    -> write('( '),
      Pos is Pos0 + 2,
      write_b1(Term, OptL, Offs, Pos),
      write(')')
    ; write_b1(Term, OptL, Offs, Pos0)
    ).

and_layout(T) :- T = (_,_).

write_b1(Term, _, Offs, Pos) :-
    prolog_listing:or_layout(Term), !,
    write_b_layout(Term, or,  Offs, Pos).
write_b1(Term, _, Offs, Pos) :-
    and_layout(Term), !,
    write_b_layout(Term, and, Offs, Pos).
write_b1(Term, OptL, _, _) :-
    write_term(Term, OptL).

write_b_layout(Term, Layout, Offs, Pos) :-
    bin_op(Term, Op, Left, Right, A, B),
    !,
    write_b(A, Left, Offs, Pos),
    LinePos is Offs + Pos,
    nl_indent(Layout, Op, LinePos),
    write_b(B, Right, Offs, Pos).

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

write_t(Term, OptionL0) :-
    merge_options([quoted(false), priority(1200)], OptionL0, OptionL),
    write_term(Term, OptionL).
