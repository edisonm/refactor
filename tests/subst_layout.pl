:- module(subst_layout, [subst_layout/2]).

subst_layout(A, B) :-
    p(A, A),
    q(B,A),
    r(B, B).

q(1,1).
p(1,1).
r(1,1).
