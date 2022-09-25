:- module(add_sentence_var, [add_sentence_var/1]).

add_sentence_var([A|B]) :-
    add_sentence_var(A),
    add_sentence_var(B).
