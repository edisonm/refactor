:- module(exapp, [exapp/3]).
:- style_check(-singleton).
exls(L) :-
    append([a], /* 0 */ [ /* 1 */ ] /* 2 */, L).
exls(L) :-
    append([a], [f(_B) /* 1 */] /*2*/, L).
exls(L) :-
    append([a], [f(b)], L).

exapp(A, T, C) :-
    append([ /*1*/A,
             /*2*/A], /*3*/ T, C).
exapp(A1-A2, T, C) :-
    append([ [ _, [ A1 ] ] ], [ [ _, [ A2 ] ], [ _, [ T ] ] ],
           C).
