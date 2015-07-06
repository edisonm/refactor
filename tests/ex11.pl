:- module(ex11, [ex11/1]).

ex11([A|B]) :-
    ex11(A),
    ex11(B).
