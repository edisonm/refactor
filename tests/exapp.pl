:- module(exapp, [exapp/3]).

exapp(A, T, C) :-
    append([ /*1*/A,
	     /*2*/A], T, C).
