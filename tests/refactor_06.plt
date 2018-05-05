:- begin_tests(refactor).

:- include(refactor_common).

:- use_module(ex6).

/* $ex6$
diff -ruN ex6.pl -
--- ex6.pl (source)
+++ ex6.pl (target)
@@ -1,7 +1,7 @@
 :- module(ex6, [q/3]).
 
 q(A, B, L) :-
-    p(A, B, L, []).
+    p(B, A, L, []).
 
 p(_, _) --> [].
-p(A, B) --> p(A, B), "hello".
+p(A, B) --> p(B, A), "hello".
*/

test(ex6) :-
    execute_test(ex6, replace_goal(p(A,B,L,T), p(B,A,L,T))).

:- end_tests(refactor).
