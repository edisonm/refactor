:- begin_tests(refactor_18).

:- include(refactor_common).

:- use_module(ex18).

/* $ex18$
diff -ruN ex18.pl -
--- ex18.pl (source)
+++ ex18.pl (target)
@@ -1,7 +1,6 @@
 :- module(ex18, [ex18/1]).
 
-ex18(C) :-
-    C=M : H,
+ex18(M : H) :-
     p(M:H).
 
 p(_C).
*/

test(ex18) :-
    execute_test(ex18, replace_sentence((H:-A=B,p(C)), (H1:-p(C1)), substitute_value(A,B,H-C,H1-C1))).

:- end_tests(refactor_18).
