:- module(ex27, [ex27/0]).

:- op(100,xfy,(@@)).

b(_).

ex27 :-
    % test1
    X = (5,2), % test2
    b(X).

ex27_2 :-
    @@(_A, _B).

@@(_, _).
