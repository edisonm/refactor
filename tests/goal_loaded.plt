:- begin_tests(goal_loaded).

:- include(refactor_common).

:- use_module(goal_loaded).

/* $goal_loaded$
diff -ruN goal_loaded.pl -
--- goal_loaded.pl (source)
+++ goal_loaded.pl (target)
@@ -7,12 +7,12 @@
     b~n".
 
 a(X) :-
-    goal_loaded:r,
-    call(r),
-    call(b(r), c, d(X)),
+    goal_loaded:r(1),
+    call(r(1)),
+    call(b(r(1)), c, d(X)),
     call(c, d).
 
-a --> b(r).
+a --> b(r(1)).
 
 d([a,b], a, b).
 
*/

test(goal_loaded) :-
    execute_test(goal_loaded, replace_goal(r,r(1))).

/* $exdcg$
diff -ruN goal_loaded.pl -
--- goal_loaded.pl (source)
+++ goal_loaded.pl (target)
@@ -9,10 +9,10 @@
 a(X) :-
     goal_loaded:r,
     call(r),
-    call(b(r), c, d(X)),
+    call(b(r, s), c, d(X)),
     call(c, d).
 
-a --> b(r).
+a --> b(r, s).
 
 d([a,b], a, b).
 
*/

test(exdcg) :-
    execute_test(goal_loaded, exdcg, replace_goal(b(r,A,B),b(r,s,A,B)), []).

:- end_tests(goal_loaded).
