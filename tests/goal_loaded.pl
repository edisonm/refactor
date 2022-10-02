:- module(goal_loaded, [a/1]).

:- meta_predicate b(0, 1, 2).

a(X) :-
    X="a~n\c
    b~n".

a(X) :-
    goal_loaded:r,
    call(r),
    call(b(r), c, d(X)),
    call(c, d).

a --> b(r).

d([a,b], a, b).

r.

c(d).

b(_, _, _).
