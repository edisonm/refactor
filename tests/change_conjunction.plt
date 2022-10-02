:- begin_tests(change_conjunction).

:- include(refactor_common).

/* $change_conjunction$
diff -ruN change_conjunction.pl -
--- change_conjunction.pl (source)
+++ change_conjunction.pl (target)
@@ -3,7 +3,9 @@
 rb :-
     call_cleanup(call,
                  % 1
-                 cleanup).
+                 ( cleanup1(a),
+                   cleanup2(a)
+                 )).
 
 call.
 
@@ -12,7 +14,8 @@
 rb2 :-
     call,
     /* 1 */
-    cleanup,
+    cleanup1(a),
+    cleanup2(a),
     done.
 
 cleanup :-
*/

test(change_conjunction) :-
    execute_test(change_conjunction, replace_conjunction(cleanup, (cleanup1(a),cleanup2(a))), [file(change_conjunction)]).

/* $change_conjunction2$
diff -ruN change_conjunction.pl -
--- change_conjunction.pl (source)
+++ change_conjunction.pl (target)
@@ -22,4 +22,4 @@
 cleanup1(_).
 
 p :-
-    forall(q, (a, b, c)).
+    forall(q, (b, c)).
*/

test(change_conjunction2) :-
    rreset,
    execute_test(change_conjunction2, replace_conjunction((a,b), b), [file(change_conjunction)]).

:- end_tests(change_conjunction).
