:- begin_tests(refactor).

:- include(refactor_common).

/* $exnoload$
diff -ruN exnoload.pl -
--- exnoload.pl (source)
+++ exnoload.pl (target)
@@ -1,7 +1,7 @@
 :- module(exnoload, [exnoload/1]).
 
 exnoload(A) :-
-    exnoload(A, 2),
-    exnoload(A, 1).
+    'exnoload*'(A, 2),
+    'exnoload*'(A, 1).
 
 exnoload(A, A).
*/

test(exnoload) :-
    execute_test(exnoload, replace_goal(exnoload(A,B), 'exnoload*'(A,B)),
                 [files(exnoload)]).

:- end_tests(refactor).
