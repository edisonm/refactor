:- begin_tests(swap_args_dcg).

:- include(refactor_common).

:- use_module(swap_args_dcg).

/* $swap_args_dcg$
diff -ruN swap_args_dcg.pl -
--- swap_args_dcg.pl (source)
+++ swap_args_dcg.pl (target)
@@ -1,7 +1,7 @@
 :- module(swap_args_dcg, [q/3]).
 
 q(A, B, L) :-
-    p(A, B, L, []).
+    p(B, A, L, []).
 
 p(_, _) --> [].
-p(A, B) --> p(A, B), "hello".
+p(A, B) --> p(B, A), "hello".
*/

test(swap_args_dcg) :-
    execute_test(swap_args_dcg, replace_goal(p(A,B,L,T), p(B,A,L,T))).

:- end_tests(swap_args_dcg).
