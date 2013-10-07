:- module(gcb, [greatest_common_binding/4,
		greatest_common_binding/7,
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

greatest_common_binding(Term0, Into0, Term, Into) :-
    common_vars(Term0, Into0, Vars0),
    greatest_common_binding(Term0, Term0, Into0, Vars0, Term, Into, _).

greatest_common_binding(SubTerm, Term0, Into0, Vars0, Term, Into, Vars) :-
    ( (var(SubTerm);var(Into0)) ->
      Term=Term0, Into=Into0
    ; substitute(Var=SubTerm, Into0, Into),
      Into0\==Into,
      substitute(Var=SubTerm, Term0, Term),
      common_vars(Term, Into, Vars),
      subtract_eq(Vars, Vars0, [])
    ->true % [Var=SubTerm]
    ; greatest_common_binding(1, SubTerm, Term0, Into0, Vars0, Term, Into, Vars),
      Into0\==Into
    ->true
    ; Term=Term0, Into=Into0
    ).

greatest_common_binding(N, SubTerm, Term0, Into0, Vars0, Term, Into, Vars) :-
    arg(N, SubTerm, Arg),
    !,
    greatest_common_binding(Arg, Term0, Into0, Vars0, Term1, Into1, Vars1),
    succ(N, N1),
    greatest_common_binding(N1, SubTerm, Term1, Into1, Vars1, Term, Into, Vars).
greatest_common_binding(_, _, Term, Into, Vars, Term, Into, Vars).

substitute_olist(Tail) --> {var(Tail)}, !.
substitute_olist([Subst|Tail]) -->
    substitute(Subst),
    substitute_olist(Tail).

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
