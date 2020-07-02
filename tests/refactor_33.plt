:- begin_tests(refactor_33).

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

test(save_changes) :-
    module_property(plunit_refactor_33, file(F)),
    absolute_file_name('ex1_.pl', Ex1, [file_type(prolog), relative_to(F)]),
    tmp_file_stream(text, File, Stream),
    close(Stream),
    copy_file(Ex1, File),
    [File],
    rreset,
    replace_term((same_term(c,a),d,b),((d,b)), [module(ex1_)]),
    with_output_to(string(Result), rshow),
    assertion(Result\==""),
    rcommit.

:- end_tests(refactor_33).
