:- begin_tests(self_refactor).

:- include(refactor_common).
:- use_module(library(filesex)).

test(save_changes) :-
    module_property(plunit_self_refactor, file(F)),
    absolute_file_name('keep_layout.pl', Ex1, [file_type(prolog), relative_to(F)]),
    tmp_file_stream(text, File, Stream),
    close(Stream),
    copy_file(Ex1, File),
    [File],
    rreset,
    replace_term((same_term(c,a),d,b),((d,b)), [module(keep_layout)]),
    with_output_to(string(Result), rshow),
    assertion(Result\==""),
    rcommit.

:- end_tests(self_refactor).
