:- module(change_operator, [change_operator/2]).

change_operator(A, (B, c)) :-
    A + B  : (A -> B).
