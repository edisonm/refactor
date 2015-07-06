:- module(ex13, [ex13/2]).

ex13(A, B) :-
    p(A, A),
    q(B,A),
    r(B, B).

q(1,1).
p(1,1).
r(1,1).
