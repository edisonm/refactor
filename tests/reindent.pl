:- module(reindent, [p/0]).

p :-
    elemof(
        [ one,
          two____, % two
          xxx,
          three,
          four
        ]).
