:- begin_tests(refspeed).

:- include(refactor_common).

% Historical tests, with 5000 lines:
%
% At 6d4db4eec3aeeb2cc5d7d7ed4ef5cdd97e050e4a:
% ERROR: /home/edison/apps/plsteroids/refactor/tests/refspeed.plt:5:
% 	test refspeed1: assertion failed
% 	Assertion: 12.98875641822815<5
%
% At a3657376a7cae0e71d09b9a506cadd8965f5fed3:
% time{cpu:0.0005743400000000065,inferences:1594,wall:3.5460288524627686}
%
% At 6ff66daa291bc9041ce72cb748f53fdf8e84d843:
% time{cpu:2.3686754769999996,inferences:19252454,wall:2.368762254714966}

test(refspeed1) :-
    rreset,
    gen_random_pred(p1(id, randatm, randstr),  5000, [file(plbin(random1))]),
    call_time(replace_term(p1(A, B, C), p2(A, B, C), [file(plbin(random1)), concurrent(false)]), Stats),
    writeln(user_error, Stats),
    Wall = Stats.wall,
    assertion(Wall < 2.5).

:- end_tests(refspeed).
