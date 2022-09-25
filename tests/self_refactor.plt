:- begin_tests(self_refactor).

:- include(refactor_common).
:- use_module(library(filesex)).

test(self_refactor_1) :-
    rreset,
    replace_term(print_expansion(A, B, C, D, E),
                 print_expansion_(A, B, C, D, E), [module(ref_replace)]),
    with_output_to(string(Result), rshow), assertion(Result \== "").

test(self_refactor_2) :-
    rreset,
    replace_term(rportray(A, B), rportray_(A, B), [module(ref_replace)]),
    with_output_to(string(Result), rshow), assertion(Result \== "").

:- end_tests(self_refactor).
