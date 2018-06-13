:- begin_tests(refactor_12).

:- include(refactor_common).

:- use_module(ex12).

/* $ex12$
diff -ruN ex12.pl -
--- ex12.pl (source)
+++ ex12.pl (target)
@@ -1,12 +1,10 @@
 :- module(ex12, [ex12/0]).
 
 ex12 :-
-    ( a  ),
     b.
 
 ex12 :-
     a,
-    a,
     b.
 
 a.
*/

test(ex12) :-
    execute_test(ex12, replace_term((a, b), b)).

:- end_tests(refactor_12).
