:- begin_tests(refactor).

:- use_module(library(refactor)).

test(example_1) :-
    [ex1],
    FResult = '/tmp/ex1.diff',
    (access_file(FResult, exist) -> delete_file(FResult) ; true),
    replace_term(ex1:_, (same_term(c,a),d,b),(d,b), diff(FResult)),
    read_file_to_codes('ex1.diff_prev', Pattern, []),
    read_file_to_codes(FResult, Result, []),
    assertion(Pattern == Result).

:- end_tests(refactor).
