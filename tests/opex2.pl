:- module(opex2, [opex2/2
                 ]).

:- op(700, xfy, myis).
:- op(700, xfy, myis2).

A myis B :- display(A myis B), nl.

opex2(A, B) :-
    A myis B.

