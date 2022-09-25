:- begin_tests(add_subterm).

:- include(refactor_common).

:- use_module(add_subterm).

/* $add_subterm_1$
diff -ruN add_subterm.pl -
--- add_subterm.pl (source)
+++ add_subterm.pl (target)
@@ -1,3 +1,3 @@
 :- module(add_subterm, [add_subterm/2]).
 
-add_subterm(f(A), g(A)).
+add_subterm(f(A), g(C, a(C))).
*/

test(add_subterm_1) :-
    execute_test(add_subterm, add_subterm_1,
                 replace_term(g(_A), g(B,X), ((X=a(B),B='$VAR'('C')))),
                 [sentence(add_subterm(_, _))]).

/* $add_subterm_2$
diff -ruN add_subterm.pl -
--- add_subterm.pl (source)
+++ add_subterm.pl (target)
@@ -1,3 +1,3 @@
 :- module(add_subterm, [add_subterm/2]).
 
-add_subterm(f(A), g(A)).
+add_subterm(f(A), g(A, f(A))).
*/

test(add_subterm_2) :-
    execute_test(add_subterm, add_subterm_2,
                 replace_term(g(A), g(A,X), true), [sentence(add_subterm(X, _))]).

:- end_tests(add_subterm).
