:- module(exnoload, [exnoload/1]).

exnoload(A) :-
    exnoload(A, 2),
    exnoload(A, 1).

exnoload(A, A).
