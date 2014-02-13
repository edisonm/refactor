:- module(gcb, [greatest_common_binding/7,
		greatest_common_binding/9,
		substitute/3]).

:- use_module(library(substitute), [substitute/5]).

greatest_common_binding(Term0, Into0, Term, Into, Skip) -->
    greatest_common_binding(Term0, _, Term0, Into0, Term, Into, Skip).

greatest_common_binding(SubTerm0, SubTerm, Term0, Into0, Term, Into, Skip) -->
    ( {nonvar(SubTerm0 ), nonvar(Into0 ), \+memberchk(SubTerm0, Skip)}
    ->( { substitute(Var=SubTerm0, Into0, Into1),
	  Into0\==Into1}
      ->{substitute(Var=SubTerm0, Term0, Term1)},
	[Var=SubTerm]
      ; {Term1=Term0, Into1 = Into0 }
      ),
      ( {compound(SubTerm0 )},
        greatest_common_binding(1, SubTerm0, SubTerm, Term1, Into1, Term, Into, Skip),
	{Into1\==Into}
      ->[]
      ; {SubTerm=SubTerm0, Term=Term1, Into=Into1 }
      )
    ; {SubTerm=SubTerm0, Term=Term0, Into=Into0 }
    ).

greatest_common_binding(N, SubTerm0, SubTerm, Term0, Into0, Term, Into, Skip) -->
    {arg(N, SubTerm0, Arg)},
    !,
    pick_tail(Tail),
    greatest_common_binding(Arg, _, Term0, Into0, Term1, Into1, Skip),
    {substitute_olist(Tail, Term1, Term2),
     substitute_olist(Tail, SubTerm0, SubTerm1),
     succ(N, N1)},
    greatest_common_binding(N1, SubTerm1, SubTerm, Term2, Into1, Term, Into, Skip).
greatest_common_binding(_, SubTerm, SubTerm, Term, Into, Term, Into, _) --> [].

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
    substitute(==, Var, Val, Term0, Term).
