:- module(swap_args_dcg, [q/3]).

q(A, B, L) :-
    p(A, B, L, []).

p(_, _) --> [].
p(A, B) --> p(A, B), "hello".
