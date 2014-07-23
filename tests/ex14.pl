:- module(ex14, [ex14/2]).

ex14([A, B], _C) :-
    A = f(B),
    true.

ex14((A, B), _C) :-
    A = B,
    true.

ex14(A, B) :-
    A = f([/**/B, _C]),
    true.

ex14(A, B) :-
    f(A, 'b') = f(a, B),
    \+ A,
    \+ B.

ex14(A, B) :-
    B = [x|T],
    ex14(A, T).

a.

b.
