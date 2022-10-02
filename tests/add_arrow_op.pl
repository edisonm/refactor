:- module(add_arrow_op, [add_arrow_op/2]).

'add_arrow_op'(A, B) :-
    once(( A=1,
           B=a
         ; A=2,
           B=b
         ; A=3,
           B=c
         )).
