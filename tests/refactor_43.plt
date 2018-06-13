:- begin_tests(refactor_43).

:- include(refactor_common).

/* $addlit$
diff -ruN addlit.pl -
--- addlit.pl (source)
+++ addlit.pl (target)
@@ -5,11 +5,13 @@
 p3(_).
 
 q1(A) :-
-    p1(A),
+    test1,
     p2,
+    p1(A),
     p3(A).
 
 q1(A) :-
     p3(A),
-    p1(A),
-    p2.
+    test1,
+    p2,
+    p1(A).
*/

test(addlit) :-
    execute_test(addlit, replace_conjunction((p1(A), p2), (test1, p2, p1(A))), [file(addlit)]).

:- end_tests(refactor_43).
