:- module(ex18, [ex18/1]).

ex18(C) :-
    C=M : H,
    p(M:H).

p(_C).
