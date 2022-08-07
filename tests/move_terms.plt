:- begin_tests(move_terms).

:- include(refactor_common).
:- use_module(library(filesex)).
:- use_module(library(readutil)).

test(move_terms) :-
    module_property(plunit_move_terms, file(F)),
    absolute_file_name(library(ref_replace), Ex1, [file_type(prolog), access(exist), relative_to(F)]),
    absolute_file_name(library(ref_message), Ex2, [file_type(prolog), access(exist), relative_to(F)]),
    tmp_file_stream(text, File1, Stream1),
    close(Stream1),
    copy_file(Ex1, File1),
    read_file_to_string(Ex1, String1, []),
    tmp_file_stream(text, File, Stream2),
    close(Stream2),
    copy_file(Ex2, File),
    read_file_to_string(Ex2, String2, []),
    rreset,
    move_term(_, [file(File1)], [file(File)], []),
    rcommit,
    read_file_to_string(File, String, []),
    assertion(string_concat(String2, String1, String)).

:- end_tests(move_terms).
