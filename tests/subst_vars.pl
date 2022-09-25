:- module(subst_vars, [subst_vars/2]).

subst_vars([A, B], _C) :-
    A = f(B),
    true.

subst_vars((A, B), _C) :-
    A = B,
    true.

subst_vars(A, B) :-
    A = f([/**/B, _C]),
    true.

subst_vars(A, B) :-
    f(A, 'b') = f(a, B),
    \+ A,
    \+ B.

subst_vars(A, B) :-
    B = [x|T],
    subst_vars(A, T).

a.

b.
