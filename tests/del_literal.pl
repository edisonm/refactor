:- module(del_literal, [del_literal/0]).

del_literal :-
    ( a  ),
    b.

del_literal :-
    a,
    a,
    b.

a.

b.
