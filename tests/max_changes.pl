:- module(maxchg, [p/1,
                   q/2,
                   r/2,
                   s/2]).

p(a).
p(b).
p(c).

s(A, B) :-
    r(A, B).

r(A, B) :-
    q(A, B).
r(A, _) :-
    q(A, A).
r(_, _) :-
    throw(exception(e)).

q(A, B) :-
    ( p(A)
    ->( p(B)
      ->writeln(B)
      ; writeln(-B)
      )
    ; writeln(-A),
      fail
    ).
