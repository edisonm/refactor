:- begin_tests(refactor).

:- include(refactor_common).

/* $maxchg$
diff -ruN maxchg.pl -
--- maxchg.pl (source)
+++ maxchg.pl (target)
@@ -8,7 +8,7 @@
 p(c).
 
 s(A, B) :-
-    r(A, B).
+    rr(A, B).
 
 r(A, B) :-
     q(A, B).
*/

test(maxchg) :-
    execute_test(maxchg, replace_term(r(A,B), rr(A,B)), [max_changes(1), file(maxchg)]).

:- end_tests(refactor).
