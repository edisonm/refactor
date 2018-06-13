:- begin_tests(refactor_38).

:- include(refactor_common).

/* $eqname_1$
diff -ruN eqname.pl -
--- eqname.pl (source)
+++ eqname.pl (target)
@@ -1,4 +1,4 @@
 :- module(eqname, [eqname/2]).
 
 eqname(A, (B, c)) :-
-    A + B  : (A -> B).
+    (A + B)  ^ (A -> B).
*/

test(eqname_1) :-
    execute_test(eqname_1, replace_term(A:B,A^B), [files(eqname)]).

/* $eqname_2$
diff -ruN eqname.pl -
--- eqname.pl (source)
+++ eqname.pl (target)
@@ -1,4 +1,4 @@
 :- module(eqname, [eqname/2]).
 
 eqname(A, (B, c)) :-
-    A + B  : (A -> B).
+    A + B  *-> (A -> B).
*/

test(eqname_2) :-
    execute_test(eqname_2, replace_term((A:B),A*->B),[files(eqname)]).

:- end_tests(refactor_38).
