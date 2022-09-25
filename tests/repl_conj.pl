:- module(repl_conj, [repl_conj/0]).

repl_conj :-
    a(C),
    b(b),
    c(C),
    d(d).
repl_conj :-
    a(a),
    b(b).

a(_).
b(_).
c(_).
d(_).
