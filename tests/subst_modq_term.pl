:- module(subst_modq_term, [subst_modq_term/1]).

subst_modq_term(C) :-
    C=M : H,
    p(M:H).

p(_C).
