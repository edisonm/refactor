:- begin_tests(refactor_11).

:- include(refactor_common).

:- use_module(ex11).

/* $ex11$
diff -ruN ex11.pl -
--- ex11.pl (source)
+++ ex11.pl (target)
@@ -1,5 +1,5 @@
 :- module(ex11, [ex11/1]).
 
 ex11([A|B]) :-
-    ex11(A),
+    ex11_one(A),
     ex11(B).
*/

test(ex11) :-
    execute_test(ex11, ex11,
                 replace_term(ex11(A), ex11_one(A), true),
                 [sentence((ex11([A|_]):-_))]).

:- end_tests(refactor_11).
