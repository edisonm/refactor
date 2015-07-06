:- module(mapargs, [mapargs/2,
		    mapargs/3,
		    mapargs/4,
		    mapargs/5,
		    mapargs/6]).

mapargs_(N, Goal, T) :-
    arg(N, T, A),
    !,
    call(Goal, N, A),
    succ(N, N1),
    mapargs_(N1, Goal, T).
mapargs_(_, _, _).

mapargs_(N, Goal, T1, T2) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    !,
    call(Goal, N, A1, A2),
    succ(N, N1),
    mapargs_(N1, Goal, T1, T2).
mapargs_(_, _, _, _).

mapargs_(N, Goal, T1, T2, T3) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    arg(N, T3, A3),
    !,
    call(Goal, N, A1, A2, A3),
    succ(N, N1),
    mapargs_(N1, Goal, T1, T2, T3).
mapargs_(_, _, _, _, _).

mapargs_(N, Goal, T1, T2, T3, T4) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    arg(N, T3, A3),
    arg(N, T4, A4),
    !,
    call(Goal, N, A1, A2, A3, A4),
    succ(N, N1),
    mapargs_(N1, Goal, T1, T2, T3, T4).
mapargs_(_, _, _, _, _, _).

mapargs_(N, Goal, T1, T2, T3, T4, T5) :-
    arg(N, T1, A1),
    arg(N, T2, A2),
    arg(N, T3, A3),
    arg(N, T4, A4),
    arg(N, T5, A5),
    !,
    call(Goal, N, A1, A2, A3, A4, A5),
    succ(N, N1),
    mapargs_(N1, Goal, T1, T2, T3, T4, T5).
mapargs_(_, _, _, _, _, _, _).

:- meta_predicate
    mapargs(2,?),
    mapargs(3,?,?),
    mapargs(4,?,?,?),
    mapargs(5,?,?,?,?),
    mapargs(6,?,?,?,?,?).

mapargs(Goal, Term)               :- mapargs_(1, Goal, Term).
mapargs(Goal, T1, T2)             :- mapargs_(1, Goal, T1, T2).
mapargs(Goal, T1, T2, T3)         :- mapargs_(1, Goal, T1, T2, T3).
mapargs(Goal, T1, T2, T3, T4)     :- mapargs_(1, Goal, T1, T2, T3, T4).
mapargs(Goal, T1, T2, T3, T4, T5) :- mapargs_(1, Goal, T1, T2, T3, T4, T5).
