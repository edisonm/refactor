:- begin_tests(gcb).

:- use_module(library(gcb)).

test(example_1) :-
    greatest_common_binding(h(f(A)),g(A,f(A)),T,I,[],B,[]),
    B = [X=Y,Z=A],
    assertion(Y == f(Z)),
    assertion(T == h(X)),
    assertion(I == g(Z, X)).

test(example_2) :-
    greatest_common_binding(f(f(f(a))),g(a,f(f(f(a))),f(f(a))),T,I,[],B,[]),
    B = [V1=T1, V2=T2, V3=T3],
    assertion(T3==a),
    assertion(T2==f(f(V3))),
    assertion(T1==f(V2)),
    assertion(V1==T),
    assertion(I==g(V3, T, V2)).

:- end_tests(gcb).
