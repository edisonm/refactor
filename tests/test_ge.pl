:- module(test_ge, [a/1]).
/*
extend_args(Goal, _, Goal) :- var(Goal), !, fail.
extend_args(M:Goal0, N, M:Goal) :- !,
    extend_args(Goal0, N, Goal).
extend_args(Goal, N, GoalEx) :-
    callable(Goal), !,
    Goal =.. List,
    length(Extra, N),
    append(List, Extra, ListEx),
    GoalEx =.. ListEx.

scan_hiord_args(Goal, Spec) :-
    arg(N, Goal, Arg),
    arg(N, Spec, SA),
    scan_hiord_arg(SA, Arg).

scan_hiord_arg(0, _) :- !.
scan_hiord_arg(^, _) :- !.
scan_hiord_arg(N, G) :-
    integer(N),
    extend_args(G, N, G1),
    ignore(goal_expansion(G1, _)).

goal_expansion(G, _) :-
    ( write(user_error, G),
      nl(user_error)
    ; '$set_source_module'(M, M),
      predicate_property(M:G, meta_predicate(Spec)),
      scan_hiord_args(G, Spec)
    ),
    fail.
*/
:- meta_predicate b(0, 1, 2).

a(X) :-
    X="a~n\c
    b~n".

a(X) :-
    test_ge:r,
    call(r),
    call(b(r), c, d(X)),
    call(c, d).

d([a,b], a, b).

r.

c(d).
