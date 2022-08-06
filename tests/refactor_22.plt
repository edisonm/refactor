:- begin_tests(refactor_22).

:- include(refactor_common).

:- use_module(ex22).

/* $ex22$
diff -ruN ex22.pl -
--- ex22.pl (source)
+++ ex22.pl (target)
@@ -1,4 +1,2 @@
 :- module(ex22, []).
 
-:- dynamic a/1  .
-
*/

test(ex22) :-
    execute_test(ex22, replace_sentence((:- dynamic _), [])).

:- end_tests(refactor_22).
