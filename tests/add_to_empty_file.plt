:- begin_tests(add_to_empty_file).

:- include(refactor_common).

/* $add_to_empty_file1$
diff -ruN add_to_empty_file.pl -
--- add_to_empty_file.pl (source)
+++ add_to_empty_file.pl (target)
@@ -0,0 +1 @@
+a.
*/

test(add_to_empty_file1) :-
    execute_test(add_to_empty_file1, replace_sentence([], [a], true), [file(add_to_empty_file)]).

/* $add_to_empty_file2$
diff -ruN add_to_empty_file.pl -
--- add_to_empty_file.pl (source)
+++ add_to_empty_file.pl (target)
@@ -0,0 +1 @@
+a.
*/

test(add_to_empty_file2) :-
    execute_test(add_to_empty_file1, replace_sentence(end_of_file, [a], true), [file(add_to_empty_file)]).

:- end_tests(add_to_empty_file).
