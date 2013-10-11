:- module(gcb, [greatest_common_binding/5,
		greatest_common_binding/8,
		substitute_list/3]).

% ?- greatest_common_binding(h(f(A)),g(A,f(A)),T,I,B,[]).
% T = h(_G357),
% I = g(A, _G357),
% B = [_G357=f(A)].

subtract_eq([], _, []).
subtract_eq([Elem0|T], L, Set0) :-
    ( member(Elem, L),
      Elem0 == Elem ->
      Set0 = Set
    ; Set0 = [Elem0|Set]
    ),
    subtract_eq(T, L, Set).

common_vars(Term, Into, Vars) :-
    term_variables(Term, VTerm),
    term_variables(Into, VInto),
    subtract_eq(VInto, VTerm, Vars).

greatest_common_binding(Term0, Into0, Term, Into, Unifier) :-
    greatest_common_binding(Term0, _, Term0, Into0, Term, Into, Unifier, []).

greatest_common_binding(SubTerm0, SubTerm, Term0, Into0, Term, Into) -->
    ( {var(SubTerm0);var(Into0 )} ->
      {SubTerm=SubTerm0, Term=Term0, Into=Into0 }
    ; ( {substitute(Var=SubTerm0, Into0, Into1),
	 Into0\==Into1}
      ->{substitute(Var=SubTerm0, Term0, Term1)},
	[Var=SubTerm]
      ; {Term1=Term0, Into1 = Into0 }
      ),
      ( greatest_common_binding(1, SubTerm0, SubTerm, Term1, Into1, Term, Into),
	{Into1\==Into}
      ->[]
      ; {SubTerm=SubTerm0, Term=Term1, Into=Into1 }
      )
    ).

greatest_common_binding(N, SubTerm0, SubTerm, Term0, Into0, Term, Into) -->
    {arg(N, SubTerm0, Arg)},
    !,
    pick_tail(Tail),
    greatest_common_binding(Arg, _, Term0, Into0, Term1, Into1),
    {substitute_olist(Tail, Term1, Term2),
     substitute_olist(Tail, SubTerm0, SubTerm1),
     succ(N, N1)},
    greatest_common_binding(N1, SubTerm1, SubTerm, Term2, Into1, Term, Into).
greatest_common_binding(_, SubTerm, SubTerm, Term, Into, Term, Into) --> [].

% Fixpoint algorithm:
substitute_olist(SubstList, Term0, Term) :-
    ( substitute_olist_(SubstList, Term0, Term1),
      Term0 \== Term1 ->
      substitute_olist(SubstList, Term1, Term)
    ; Term0 = Term
    ).

substitute_olist_(Tail) --> {var(Tail)}, !.
substitute_olist_([Subst|Tail]) -->
    substitute(Subst),
    substitute_olist_(Tail).

substitute(Var=Val, Term0, Term) :-
    ( Term0 == Val -> Term = Var
    ; var(Term0) -> Term = Term0
    ; functor(Term0, F, A),
      functor(Term,  F, A),
      substitute(1, Var=Val, Term0, Term)
    ).

substitute(N, Subst, Term0, Term) :-
    arg(N, Term0, Arg0),
    !,
    substitute(Subst, Arg0, Arg),
    arg(N, Term, Arg),
    succ(N, N1),
    substitute(N1, Subst, Term0, Term).
substitute(_, _, _, _).

substitute_list([Subst|Tail]) -->
    substitute(Subst),
    substitute_list(Tail).
substitute_list([]) --> [].

pick_tail(Tail, Tail, Tail).
