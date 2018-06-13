:- begin_tests(refactor_16).

:- include(refactor_common).

:- use_module(ex16).

/* $ex16$
*/

test(ex16) :-
    execute_test(ex16, replace_sentence(H, H, true)).

:- end_tests(refactor_16).
