:- module(addlit, [q1/1]).

p1(_).
p2.
p3(_).

q1(A) :-
    p1(A),
    p2,
    p3(A).

q1(A) :-
    p3(A),
    p1(A),
    p2.
