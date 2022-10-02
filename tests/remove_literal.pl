:- module(remove_literal, [remove_literal/3]).

remove_literal(L, PL, R) :-
    findall([A,B],
            ( member([A, B, C], L),
              nth1(P, L, [A,B,C]),
              \+ member(P, PL)
            ), R).
