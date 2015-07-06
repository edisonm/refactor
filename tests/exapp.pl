:- module(exapp, [exapp/3]).

exapp(A, T, C) :-
    append([ /*1*/A,
	     /*2*/A], T, C).
exapp(A1-A2, T, C) :-
    append([ [ _, [ A1 ] ] ], [ [ _, [ A2 ] ], [ _, [ T ] ] ],
	   C).
