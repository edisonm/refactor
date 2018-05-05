:- begin_tests(refactor).

:- include(refactor_common).

/* $ref_body$
diff -ruN ref_body.pl -
--- ref_body.pl (source)
+++ ref_body.pl (target)
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

test(ref_body) :-
    execute_test(ref_body, replace_conjunction(cleanup, (cleanup1(a),cleanup2(a))), [file(ref_body)]).

/* $ref_body2$
diff -ruN ref_body.pl -
--- ref_body.pl (source)
+++ ref_body.pl (target)
@@ -22,4 +22,4 @@
 cleanup1(_).
 
 p :-
-    forall(q, (a, b, c)).
+    forall(q, (b, c)).
*/

test(ref_body2) :-
    rreset,
    execute_test(ref_body2, replace_conjunction((a,b), b), [file(ref_body)]).

:- end_tests(refactor).
