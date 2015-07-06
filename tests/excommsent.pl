:- module(excommsent, []).

% comment

f(a, /*1*/[C]).

g(a,b,c) :-
    % comment
    f(a,b,c), %,
    g.
