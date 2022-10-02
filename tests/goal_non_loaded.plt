:- begin_tests(goal_non_loaded).

:- include(refactor_common).

/* $goal_non_loaded$
diff -ruN goal_non_loaded.pl -
--- goal_non_loaded.pl (source)
+++ goal_non_loaded.pl (target)
@@ -1,7 +1,7 @@
 :- module(goal_non_loaded, [goal_non_loaded/1]).
 
 goal_non_loaded(A) :-
-    goal_non_loaded(A, 2),
-    goal_non_loaded(A, 1).
+    'goal_non_loaded*'(A, 2),
+    'goal_non_loaded*'(A, 1).
 
 goal_non_loaded(A, A).
*/

test(goal_non_loaded) :-
    execute_test(goal_non_loaded, replace_goal(goal_non_loaded(A,B), 'goal_non_loaded*'(A,B)),
                 [files(goal_non_loaded)]).

:- end_tests(goal_non_loaded).
