:- module(goal_non_loaded, [goal_non_loaded/1]).

goal_non_loaded(A) :-
    goal_non_loaded(A, 2),
    goal_non_loaded(A, 1).

goal_non_loaded(A, A).
