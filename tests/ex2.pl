:- module(ex2, []).

q(A, B, L) :-
    p(A, B, L, []).

p(_, _) --> [].
p(A, B) --> p(A, B), "hello".
