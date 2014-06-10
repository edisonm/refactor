:- module(ex24, [ex24/1]).

ex24(A) :-
    A = key_components/4+ (  hidden, kbmask([+, +, -, -]) ).
