:- module(substitute, [substitute/5, is_subterm/2]).

:- meta_predicate substitute(2, ?, ?, ?, ?).
substitute(Comp, Subs, Find, Term0, Term) :-
    ( call(Comp, Term0, Find)
    ->Term = Subs
    ; compound(Term0)
    ->functor(Term0, F, A),
      functor(Term,  F, A),
      substitute(1, Comp, Subs, Find, Term0, Term)
    ; Term = Term0
    ).

substitute(N, Comp, Subs, Find, Term0, Term) :-
    arg(N, Term0, Arg0),
    !,
    substitute(Comp, Subs, Find, Arg0, Arg),
    arg(N, Term, Arg),
    succ(N, N1),
    substitute(N1, Comp, Subs, Find, Term0, Term).
substitute(_, _, _, _, _, _).

is_subterm(SubTerm, Term) :-
    substitute(==, Var, SubTerm, Term, Term1),
    occurrences_of_var(Var, Term1, N),
    !,
    N > 0.
