:- begin_tests(refactor).

:- include(refactor_common).

/* $opfp$
diff -ruN opfp.pl -
--- opfp.pl (source)
+++ opfp.pl (target)
@@ -1,4 +1,4 @@
 :- module(opfp, [opfp/1]).
 
 opfp(X) :-
-    X is 1 +  2 +   3 +    4.
+    X is ((1 ^  2) ^   3) ^    4.
*/

test(opfp) :-
    execute_test(opfp, replace_term(A+B, A^B),[files(opfp)]).

:- end_tests(refactor).
