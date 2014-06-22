:- module(ref_context,
	  [refactor_context/2
	   ]).

%%	refactor_context(?Name, ?Value) is nondet.

refactor_context(variable_names, VarNames) :-
    b_getval(refactor_variable_names, VarNames).
refactor_context(pattern, Pattern) :-
    b_getval(refactor_pattern, Pattern).
refactor_context(into, Into) :-
    b_getval(refactor_into, Into).
refactor_context(sentence, Sentence) :-
    b_getval(refactor_sentence, Sentence).
refactor_context(goal_args, Sentence) :-
    b_getval(refactor_goal_args, Sentence).