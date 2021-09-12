:- begin_tests(refactor_54).

:- include(refactor_common).

/* $ex54$
diff -ruN ex54.pl -
--- ex54.pl (source)
+++ ex54.pl (target)
@@ -1 +1 @@
-a. /*b*/b. /*c*/c.
+/*b*/b. a. /*c*/c.
*/

test(ex54) :-
    execute_test(ex54, replace_sentence([a,b,X],[b,a,X]),[file(ex54)]).

:- end_tests(refactor_54).
