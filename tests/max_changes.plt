:- begin_tests(max_changes).

:- include(refactor_common).

/* $max_changes$
diff -ruN max_changes.pl -
--- max_changes.pl (source)
+++ max_changes.pl (target)
@@ -8,7 +8,7 @@
 p(c).
 
 s(A, B) :-
-    r(A, B).
+    rr(A, B).
 
 r(A, B) :-
     q(A, B).
*/

test(max_changes) :-
    execute_test(max_changes, replace_term(r(A,B), rr(A,B)), [max_changes(1), file(max_changes)]).

:- end_tests(max_changes).
