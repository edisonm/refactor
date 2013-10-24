:- module(ex15, [ex15/2]).

ex15([A|_B],A).
ex15([A,_B],A).
ex15({A,_B},A).
ex15((A,_B),A).
