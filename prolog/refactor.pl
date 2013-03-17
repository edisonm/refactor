:- module(refactor, [refactor/2,
		     rename_variable/4,
		     replace_term/4,
		     replace_goal/4,
		     replace_sentence/4,
		     expand_term/4,
		     expand_goal/4,
		     expand_sentence/4,
		     replace_term_id/4,
		     unfold_goal/3,
		     rename_predicate/3]).

:- use_module(library(readutil)).
:- use_module(library(prolog_codewalk)).
:- use_module(tools(file_changes)).
:- use_module(tools(term_info)).

:- dynamic file_commands_db/2.

:- meta_predicate refactor(1,+).
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
			    rename_variable_helper(OldName, NewName)),
		Action).

rename_variable_helper(OldName, NewName, T, Dict, _C, _, E) :-
    \+ memberchk(NewName=_,Dict),
    memberchk(OldName=V,Dict),
    V==T,
    E='$VAR'(NewName).

:- meta_predicate replace_term(?,?,?,+).
replace_term_id(Caller, Term, Target, Action) :-
    replace_term(Caller, Term, Target, Action),
    functor(Term, F0, A0),
    functor(Target, F, A),
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

rename_predicate_helper(P0, H0, H, T, _, _:T0, P, E) :-
    nonvar(T),
    T==T0,
    ( T = P0 ->
      P = H0, E = H
    ; T = (P0 :- _) ->
      P = (H0 :- B), E = (H :- B)
    ; T = (M:P0 :- B) ->
      P = (M:H0 = B), E = (M:H :- B)
    ).

replace(Level, Caller, Term0, Target, Action) :-
    level_term(Level, Term0, Term),
    copy_term(Term0, Ref),
    refactor(meta_expansion(Level, Caller, Ref,
			    source_expansion_helper(Caller, Term, Target)),
		Action).

replace_term(Caller, Term, Target, Action) :-
    replace(term, Caller, Term, Target, Action).

replace_sentence(Caller, Term, Target, Action) :-
    replace(sent, Caller, Term, Target, Action).

:- meta_predicate replace_goal(?,0,?,+).
replace_goal(Caller, Term, Target, Action) :-
    replace(goal, Caller, Term, Target, Action).

level_term(goal, _:Term, Term) :- !.
level_term(_,      Term, Term).

:- meta_predicate expand(+,?,?,5,-).
% Expander(+Term, +Dict, +Caller, -Pattern, -Expansion)
expand(Level, Caller, Term, Expander, Action) :-
    refactor(meta_expansion(Level, Caller, Term, Expander), Action).

expand_term(Caller, Term, Expander, Action) :-
    expand(term, Caller, Term, Expander, Action).

expand_sentence(Caller, Term, Expander, Action) :-
    expand(sent, Caller, Term, Expander, Action).

:- meta_predicate expand_goal(?,0,?,+).
expand_goal(Caller, Term, Expander, Action) :-
    expand(goal, Caller, Term, Expander, Action).

% rename_predicate(Module:F/A, NewName, Action) :-
%     refactor(source_expansion(term, Term, Module:Term, Target), Action).

:- meta_predicate unfold_goal(?,0,+).
% NOTE: Only works if exactly one clause match
unfold_goal(Module, MGoal, Action) :-
    findall(clause(MGoal, Body0), clause(MGoal, Body0), [clause(MGoal, Body0)]),
    MGoal = M:_,
    (Module == M -> Body = Body0 ; Body = M:Body),
    replace_goal(MGoal, Module:_, Body, Action).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RULES (1st argument of refactor/2):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate meta_expansion(+,?,?,5,-).
% Expander(+Term, +Dict, +Caller, -Pattern, -Expansion)
meta_expansion(Level, Caller, Term, Expander, FileChanges) :-
    collect_expansion_commands(Level, Caller, Term, Expander, FileCommands),
    apply_file_commands(FileCommands, FileChanges).

collect_expansion_commands(goal, Caller, Term, Expander, FileCommands) :- !,
    prolog_walk_code([trace_reference(Term),
		      infer_meta_predicates(false),
		      evaluate(false),
		      on_trace(collect_file_commands(Caller, Expander))]),
    findall(File-Commands, retract(file_commands_db(File, Commands)), FileCommands).
collect_expansion_commands(Level, Caller, Term, Expander, FileCommands) :-
    style_check(-atom),
    findall(File-Commands,
	    get_file_commands(Level, Caller, Term, Expander, File, Commands),
	    FileCommands).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ANCILLARY PREDICATES:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

source_expansion_helper(Caller, Term, Expansion, _, _, Caller, Term, Expansion).

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

:- meta_predicate get_file_commands(?, ?,?,5,-,-).
get_file_commands(Level, M:Caller, Ref, Expander, File, Commands) :-
    current_module(M),
    M \= user,
    module_property(M, class(user)),
    get_term_info(M, Caller, File, Term, [variable_names(Dict),
					  subterm_positions(TermPos)]),
    substitute_term_level(Level, TermPos, Term, 1200, Dict, M:Caller, Ref,
			  Expander, Commands, []).

:- meta_predicate substitute_term_level(+,?,?,?,+,?,?,5,?,?).
substitute_term_level(sent, TermPos, Term, Priority, Dict, Caller, Ref,
		      Expander) -->
    {subsumes(Ref, Term)},
    substitute_term(Expander, Term, Priority, Dict, Caller, TermPos),
    !.
substitute_term_level(term, TermPos, Term, Priority, Dict, Caller, Ref,
		      Expander) -->
    substitute_term_rec(TermPos, Term, Priority, Dict, Caller, Ref, Expander).

substitute_term_rec(TermPos, Term, Priority, Dict, Caller, Ref, Expander) -->
    {subsumes(Ref, Term)},
    substitute_term(Expander, Term, Priority, Dict, Caller, TermPos),
    !.
substitute_term_rec(TermPos, Term, _, Dict, Caller, Ref, Expander) -->
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
      substitute_term_rec(PA, Arg, Priority, Dict, Caller, Ref, Expander)
    ; {N is N0 + 1},
      substitute_term_args(PAs, N, Term, Dict, Caller, Ref, Expander)
    ).

:- meta_predicate substitute_term_list(?,?,?,+,?,?,5,?,?).
substitute_term_list([EP|EPs], TP, [Elem|Term], Dict, Caller, Ref, Expander) -->
    ( {term_priority([_|_], 1, Priority)},
      substitute_term_rec(EP, Elem, Priority, Dict, Caller, Ref, Expander)
    ; substitute_term_list(EPs, TP, Term, Dict, Caller, Ref, Expander)
    ).
substitute_term_list([], TP, Tail, Dict, Caller, Ref, Expander) -->
    {term_priority([_|_], 2, Priority)},
    substitute_term_rec(TP, Tail, Priority, Dict, Caller, Ref, Expander).

:- export(collect_file_commands/5).
% NOTE: Goal and Caller unified here to improve performance -- EMM
:- meta_predicate collect_file_commands(?,4,?,?,?).
collect_file_commands(Caller, Expander, Ref, Caller, From) :-
    Dict = [],			% TODO: Calculate Dict
    calculate_commands(Expander, Ref, Dict, Caller, From, File, Commands, []),
    assertz(file_commands_db(File, Commands)).

:- multifile
    prolog:message//1,
    prolog:message_location//1.

prolog:message(acheck(refactor(Goal, From))) -->
    prolog:message_location(From),
    ['Unable to refactor ~w, no term position information available'-[Goal], nl].

:- meta_predicate calculate_commands(4,?,?,?,?,?,?,?).
calculate_commands(Expander, M:Term, Dict, Caller, From, File) -->
    { From = clause_term_position(ClauseRef, TermPos) ->
      clause_property(ClauseRef, file(File))
    ; From = file_term_position(File, TermPos) -> true
    ; print_message(error, acheck(refactor(M:Term, From))),
      fail
    },
    substitute_term(Expander, Term, 1200, Dict, Caller, TermPos).

:- meta_predicate substitute_term(4,?,?,?,?,?,?,?).
substitute_term(Expander, Term, Priority, Dict, Caller, TermPos) -->
    {calculate_expansion(Expander, Term, Dict, Caller, TermPos,
			 Params, Pattern, Expansion)},
    expansion_commands_term(TermPos, Params, Term, Priority, Pattern, Expansion).

:- meta_predicate calculate_expansion(4,?,?,?,?,?,-,-).
calculate_expansion(Expander, Term, Dict, Caller, TermPos,
		    Params, Pattern, Expansion) :-
    call(Expander, Term, Dict, Caller, Pattern, Expansion),
    \+ Pattern \= Term,
    subst_term(TermPos, Params, Pattern, Term).

valid_op_type_arity(xf,  1).
valid_op_type_arity(yf,  1).
valid_op_type_arity(xfx, 2).
valid_op_type_arity(xfy, 2).
valid_op_type_arity(yfx, 2).
valid_op_type_arity(fy,  1).
valid_op_type_arity(fx,  1).

expansion_commands_term(term_position(_, _, FFrom, FTo, SubPos),
			Params, Term, Priority, Pattern, Expansion) -->
    { nonvar(Pattern),
      Pattern \= '$substitute_by'(_, _),
      nonvar(Expansion),
      Expansion \= '$substitute_by'(_, _),
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
      [FFrom-[print(Params, Priority, FE, FTo)]]
    ; [] %% Do nothing to preserve functor layout
    ),
    expansion_commands_args(1, Params, Term, Pattern, Expansion, SubPos).
expansion_commands_term(list_position(_, _, Elms, Tail), Params, Term, _,
			Pattern, Expansion) -->
    {term_priority([_|_], 1, Priority)},
    expansion_commands_list(Elms, Tail, Params, Term, Priority, Pattern, Expansion),
    !.
expansion_commands_term(none, _, _, _, _, _) --> !, [].
expansion_commands_term(TermPos, Params, _, Priority, Pattern, Expansion) --> % Overwrite layout
    { arg(1, TermPos, From),	% BUG: can drop comments
      arg(2, TermPos, To)
    },
    ( {Pattern == Expansion} -> []
    ; [From-[print(Params, Priority, Expansion, To)]]
    ).

expansion_commands_list([], Tail, Params, Term, _, Pattern, Expansion) -->
    {term_priority([_|_], 2, Priority)},
    expansion_commands_term(Tail, Params, Term, Priority, Pattern, Expansion).
expansion_commands_list([Pos|Poss], Tail, Params, [T|Ts], Priority, [P|Ps], Expansion) -->
    { nonvar(Expansion),
      Expansion = [E|Es]
    },
    expansion_commands_term(Pos, Params, T, Priority, P, E),
    expansion_commands_list(Poss, Tail, Params, Ts, Priority, Ps, Es).

expansion_commands_args(N, Params, Term, Pattern, Expansion, [ArgPos|SubPos]) -->
    { arg(N, Pattern, PArg), !,
      arg(N, Expansion, EArg),
      arg(N, Term, TArg),
      term_priority(Term, N, Priority)
    },
    expansion_commands_term(ArgPos, Params, TArg, Priority, PArg, EArg),
    {succ(N, N1)},
    expansion_commands_args(N1, Params, Term, Pattern, Expansion, SubPos).
expansion_commands_args(_, _, _, _, _, _) --> [].

subst_args(N, Pattern, Term, Params, [ArgPos|SubPos]) :-
    arg(N, Pattern, PArg), !,
    arg(N, Term, Arg),
    subst_term(ArgPos, Params, PArg, Arg),
    succ(N, N1),
    subst_args(N1, Pattern, Term, Params, SubPos).
subst_args(_, _, _, _, _).

subst_list([], Tail, Params, E, C) :-
    subst_term(Tail, Params, E, C).
subst_list([Pos|Poss], Tail, Params, [E|Es], [C|Cs]) :-
    subst_term(Pos, Params, E, C),
    subst_list(Poss, Tail, Params, Es, Cs).

subst_term(ArgPos, Params, Var, _) :- var(Var), !,
    Var = '$substitute_by'(Params, ArgPos).
subst_term(_, _, '$substitute_by'(_, _), _) :- !. %Avoid aliasing loops
subst_term(term_position(_, _, _, _, CP), Params, Term, CTerm) :- !,
    subst_args(1, Term, CTerm, Params, CP).
subst_term(list_position(_, _, Elms, Tail), Params, Term, CTerm) :- !,
    subst_list(Elms, Tail, Params, Term, CTerm).
subst_term(none, _, T, T) :- !.
subst_term(_,    _, T, T).

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
rportray('$substitute_by'(Params, ArgPos), _) :-
    Params = Pos0-Remaining0,
    arg(1, ArgPos, From),
    arg(2, ArgPos, To),
    cut_text(Pos0, From, Remaining0, Remaining1, _),
    cut_text(From, To, Remaining1, _, Text),
    string_to_list(Text, List),
    maplist(put, List),
    !.
rportray('$substitute_by'(_, _), _).

:- use_module(library(listing),[]).

apply_change(print(Params, Priority, Term, SkipTo), Pos1, Text, File,
	     text_desc(Pos0, Remaining0, Tail0),
	     text_desc(Pos,  Remaining,  Tail)) :-
    cut_text(Pos0, Pos1, Remaining0, Remaining1, CutText), % 
    append(CutText, Tail1, Tail0),			   % Accept
    findall(t(Tail1, Tail, Pos),
	    ( Params = 0-Text,
	      numbervars(Term, 0, _, [singletons(true)]),
	      with_output_to(codes(Tail1, Tail),
			     print_expansion(Term, Priority, File,
					     Pos1, SkipTo, Pos)
			    )),
	    [t(Tail1, Tail, Pos)]),		   % Apply
    cut_text(Pos1, Pos, Remaining1, Remaining, _). % Skip

inc(Delta, Pos0, Pos) :- Pos is Pos0 + Delta.

print_expansion_list([], _, _, _) --> [].
print_expansion_list([T|L], N, File, Pos0) -->
    print_expansion(T, N, File, Pos0),
    print_expansion_list(L, N, File, Pos0).

%% print_expansion(?Term:term, N:integer, File:atom, Pos0:integer, SkipTo:integer, Pos:integer).

print_expansion(Term, N, _, _) --> {var(Term), !, write_r(Term, N)}, [].
print_expansion('$LIST'(List), N, File, Pos0) -->
    print_expansion_list(List, N, File, Pos0).
print_expansion('$TEXT'(Term), N, File, Pos0) -->
    print_expansion('$TEXT'(Term, 0), N, File, Pos0).
print_expansion('$TEXT'(Term, Delta), N, _, _) -->
    {write_term(Term, [spacing(next_argument),
		       numbervars(true),
		       priority(N)
		      ])},
    inc(Delta).
print_expansion(Term, N, _, _) --> {write_r(Term, N)}.

write_r(Term, N) :-
    write_r_2(Term, N, rportray).

:- meta_predicate write_r_2(?,+,2).
write_r_2(Term, N, PortrayGoal) :-
    Options = [portray_goal(PortrayGoal),
	       spacing(next_argument),
	       numbervars(true),
	       quoted(true),
	       priority(N)
	      ],
    write_term(Term, Options).
