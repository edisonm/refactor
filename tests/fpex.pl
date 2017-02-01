:- module(fpex, [fpex/2]).

'fpex'(A, B) :-
    once(( A=1,
           B=a
         ; A=2,
           B=b
         ; A=3,
           B=c
         )).
