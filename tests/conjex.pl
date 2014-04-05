:- module(conjex, [conjex/0]).

conjex :-
    a(C),
    b(b),
    c(C),
    d(d).
conjex :-
    a(a),
    b(b).

a(_).
b(_).
c(_).
d(_).
