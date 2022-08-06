:- begin_tests(ident_sentence).

:- include(refactor_common).

:- use_module(ident_sentence).

/* $ident_sentence$
*/

test(ident_sentence) :-
    execute_test(ident_sentence, replace_sentence(H, H, true)).

:- end_tests(ident_sentence).
