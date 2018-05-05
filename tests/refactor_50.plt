:- begin_tests(refactor).

:- include(refactor_common).

/* $empty1$
diff -ruN empty.pl -
--- empty.pl (source)
+++ empty.pl (target)
@@ -0,0 +1 @@
+a.
*/

test(empty1) :-
    execute_test(empty1, replace_sentence([], [a], true), [file(empty)]).

/* $empty2$
diff -ruN empty.pl -
--- empty.pl (source)
+++ empty.pl (target)
@@ -0,0 +1 @@
+a.
*/

test(empty2) :-
    execute_test(empty1, replace_sentence(end_of_file, [a], true), [file(empty)]).

:- end_tests(refactor).
