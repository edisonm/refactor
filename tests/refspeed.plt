:- begin_tests(refspeed).

:- include(refactor_common).

test(refspeed1) :-
    rreset,
    gen_random_pred(p1(id, randatm, randstr), 5000, [file(plbin(random1))]),
    call_time(replace_term(p1(A,B,C), p2(A,B,C), [file(plbin(random1))]), Statistics),
    Wall = Statistics.wall,
    writeln(user_error, Statistics),
    assertion(Wall < 5).

:- end_tests(refspeed).
