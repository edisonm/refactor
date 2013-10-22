:- module(ex14, [ex14/2]).

ex14(A, B) :-
    A = f(/**/B),
    true.

ex14(A, B) :-
    f(A, 'b') = f(a, B),
    \+ A,
    \+ B.

a.

a(1).

b.
