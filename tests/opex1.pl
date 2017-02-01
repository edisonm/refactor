:- module(opex1, [opex1/2
                 ]).

:- op(700, xfy, myis).
:- op(700, xfy, myis2).

A myis B :- display(A myis B), nl.

opex1(A, B) :-
    A myis B.

