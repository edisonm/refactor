:- begin_tests(refactor_58).

:- include(refactor_common).

/* $listb$
diff -ruN listb.pl -
--- listb.pl (source)
+++ listb.pl (target)
@@ -1,3 +1,4 @@
 f(A, B).
-f(A, [B,C]).
+f(A, [B,
+      C]).
 f(A, [B|C]).
*/

test(listb) :-
    execute_test(listb, replace_term(f(A,B), f(A,'$LISTB,NL'(B))),[file(listb)]).

:- end_tests(refactor_58).
