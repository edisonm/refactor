:- module(eqname, [eqname/2]).

eqname(A, (B, c)) :-
    A + B  : (A -> B).
