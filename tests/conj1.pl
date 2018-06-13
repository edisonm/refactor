:- module(conj1, [conj1/3]).

conj1(L, PL, R) :-
    findall([A,B],
            ( member([A, B, C], L),
              nth1(P, L, [A,B,C]),
              \+ member(P, PL)
            ), R).
