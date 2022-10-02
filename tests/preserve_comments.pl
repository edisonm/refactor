:- module(preserve_comments, [preserve_comments/0]).

:- op(100,xfy,(@@)).

b(_).

preserve_comments :-
    % test1
    X = (5,2), % test2
    b(X).

preserve_comments_2 :-
    @@(_A, _B).

@@(_, _).
