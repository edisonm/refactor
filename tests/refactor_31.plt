:- begin_tests(refactor).

:- include(refactor_common).

:- use_module(exge).

/* $exge$
diff -ruN exge.pl -
--- exge.pl (source)
+++ exge.pl (target)
@@ -7,12 +7,12 @@
     b~n".
 
 a(X) :-
-    exge:r,
-    call(r),
-    call(b(r), c, d(X)),
+    exge:r(1),
+    call(r(1)),
+    call(b(r(1)), c, d(X)),
     call(c, d).
 
-a --> b(r).
+a --> b(r(1)).
 
 d([a,b], a, b).
 
*/

test(exge) :-
    execute_test(exge, replace_goal(r,r(1))).

/* $exdcg$
diff -ruN exge.pl -
--- exge.pl (source)
+++ exge.pl (target)
@@ -9,10 +9,10 @@
 a(X) :-
     exge:r,
     call(r),
-    call(b(r), c, d(X)),
+    call(b(r, s), c, d(X)),
     call(c, d).
 
-a --> b(r).
+a --> b(r, s).
 
 d([a,b], a, b).
 
*/

test(exdcg) :-
    execute_test(exge, exdcg, replace_goal(b(r,A,B),b(r,s,A,B)), []).

:- end_tests(refactor).
