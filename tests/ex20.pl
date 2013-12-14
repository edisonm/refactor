:- module(ex20, [ex20/0, ex20/1]).

ex20([a,b,c]).

ex20 :-
    ex20([A|_]),
    ex20([A,B|C]),
    ex20([A,B,C]).
