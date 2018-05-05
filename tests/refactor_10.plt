:- begin_tests(refactor).

:- include(refactor_common).

:- use_module(ex10).

/* $ex10_1$
diff -ruN ex10.pl -
--- ex10.pl (source)
+++ ex10.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex10, [ex10/2]).
 
-ex10(f(A), g(A)).
+ex10(f(A), g(C, a(C))).
*/

test(ex10_1) :-
    execute_test(ex10, ex10_1,
                 replace_term(g(_A), g(B,X), ((X=a(B),B='$VAR'('C')))),
                 [sentence(ex10(_, _))]).

/* $ex10_2$
diff -ruN ex10.pl -
--- ex10.pl (source)
+++ ex10.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex10, [ex10/2]).
 
-ex10(f(A), g(A)).
+ex10(f(A), g(A, f(A))).
*/

test(ex10_2) :-
    execute_test(ex10, ex10_2,
                 replace_term(g(A), g(A,X), true), [sentence(ex10(X, _))]).

:- end_tests(refactor).
