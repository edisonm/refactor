:- begin_tests(refactor).

:- include(refactor_common).

:- use_module(ex17).

/* $ex17$
diff -ruN ex17.pl -
--- ex17.pl (source)
+++ ex17.pl (target)
@@ -1,8 +1,7 @@
 :- module(ex17, [ex17/0]).
 
 ex17 :-
-    a,
-    ( b  ).
+    a.
 
 a.
 
*/

test(ex17) :-
    execute_test(ex17, replace_sentence((H:-(A,_B)), (H:-A))).

:- end_tests(refactor).
