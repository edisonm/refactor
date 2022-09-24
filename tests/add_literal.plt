:- begin_tests(add_literal).

:- include(refactor_common).

/* $add_literal$
diff -ruN add_literal.pl -
--- add_literal.pl (source)
+++ add_literal.pl (target)
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

test(add_literal) :-
    execute_test(add_literal, replace_conjunction((p1(A), p2), (test1, p2, p1(A))), [file(add_literal)]).

:- end_tests(add_literal).
