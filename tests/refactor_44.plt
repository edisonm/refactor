:- begin_tests(refactor_44).

:- include(refactor_common).
:- use_module(library(clambda)).

/* $autovn$
diff -ruN autovn.pl -
--- autovn.pl (source)
+++ autovn.pl (target)
@@ -1,5 +1,5 @@
 :- module(autovn, [f/6]).
 
-f(X, X, _, _, _Y, _Z, V1/V1).
+f(_X, V2, V2, _, Y, Y, _Z, V1/V1).
 
 p(g( _), _Y).
*/

test(autovn) :-
    execute_test(autovn,
                  \ Options
                 ^( replace_sentence(f(A1, _A2, A3, A4, A5, A6, A7),
                                     f(A1, A3, A3, A4, A5, A5, A6, A7),
                                     Options),
                    remove_underscore_multi(Options),
                    underscore_singletons(Options)
                  ),
                  [file(autovn)]).

/* $autovn2$
diff -ruN autovn.pl -
--- autovn.pl (source)
+++ autovn.pl (target)
@@ -2,4 +2,4 @@
 
 f(X, X, _, _, _Y, _Z, V1/V1).
 
-p(g( _), _Y).
+p(g( _), f(_)).
*/

test(autovn2) :-
    execute_test(autovn2,
                  \ Options
                 ^( replace_sentence(p(C,X), p(C,X), (X=f(_A)), Options),
                    anonymize_singletons(Options)
                  ),
                  [file(autovn)]).

/* $autovn3$
diff -ruN autovn.pl -
--- autovn.pl (source)
+++ autovn.pl (target)
@@ -2,4 +2,4 @@
 
 f(X, X, _, _, _Y, _Z, V1/V1).
 
-p(g( _), _Y).
+p(g( V1), V1).
*/

test(autovn3) :-
    execute_test(autovn3, replace_sentence(p(g(A),_B), p(g(A),A)),
                  [file(autovn)]).

/* $autovn4$
diff -ruN autovn.pl -
--- autovn.pl (source)
+++ autovn.pl (target)
@@ -2,4 +2,4 @@
 
 f(X, X, _, _, _Y, _Z, V1/V1).
 
-p(g( _), _Y).
+p(g( V1), t(V1)).
*/

test(autovn4) :-
    execute_test(autovn4, replace_sentence(p(g(A),_B), p(g(A),t(A))),
                  [file(autovn)]).

:- end_tests(refactor_44).
