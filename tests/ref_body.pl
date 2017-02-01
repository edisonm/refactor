:- module(ref_body, [rb/0]).

rb :-
    call_cleanup(call,
                 % 1
                 cleanup).

call.

done.

rb2 :-
    call,
    /* 1 */
    cleanup,
    done.

cleanup :-
    cleanup1(A),
    cleanup2(A).

cleanup1(_).

p :-
    forall(q, (a, b, c)).
