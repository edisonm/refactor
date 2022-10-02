:- begin_tests(auto_variable_name).

:- include(refactor_common).
:- use_module(library(clambda)).

/* $auto_variable_name2$
diff -ruN auto_variable_name1.pl -
--- auto_variable_name1.pl (source)
+++ auto_variable_name1.pl (target)
@@ -1 +1 @@
-p(_Y, g( _)).
+p(f(_), g( _)).
*/

test(auto_variable_name2) :-
    execute_test(auto_variable_name2,
                 \ Options
                 ^( replace_sentence(p(X,C), p(X,C), (X=f(_A)), Options),
                    anonymize_singletons(Options)
                  ),
                 [file(auto_variable_name1)]).

/* $auto_variable_name$
diff -ruN auto_variable_name.pl -
--- auto_variable_name.pl (source)
+++ auto_variable_name.pl (target)
@@ -1,5 +1,5 @@
 :- module(auto_variable_name, [f/6]).
 
-f(X, X, _, _, _Y, _Z, V1/V1).
+f(_X, V2, V2, _, Y, Y, _Z, V1/V1).
 
 p(g( _), _Y).
*/

test(auto_variable_name) :-
    execute_test(auto_variable_name,
                 \ Options
                 ^( replace_sentence(f(A1, _A2, A3, A4, A5, A6, A7),
                                     f(A1, A3, A3, A4, A5, A5, A6, A7),
                                     Options),
                    remove_underscore_multi(Options),
                    underscore_singletons(Options)
                  ),
                 [file(auto_variable_name)]).

/* $auto_variable_name3$
diff -ruN auto_variable_name.pl -
--- auto_variable_name.pl (source)
+++ auto_variable_name.pl (target)
@@ -2,4 +2,4 @@
 
 f(X, X, _, _, _Y, _Z, V1/V1).
 
-p(g( _), _Y).
+p(g( V1), V1).
*/

test(auto_variable_name3) :-
    execute_test(auto_variable_name3, replace_sentence(p(g(A),_B), p(g(A),A)),
                  [file(auto_variable_name)]).

/* $auto_variable_name4$
diff -ruN auto_variable_name.pl -
--- auto_variable_name.pl (source)
+++ auto_variable_name.pl (target)
@@ -2,4 +2,4 @@
 
 f(X, X, _, _, _Y, _Z, V1/V1).
 
-p(g( _), _Y).
+p(g( V1), t(V1)).
*/

test(auto_variable_name4) :-
    execute_test(auto_variable_name4, replace_sentence(p(g(A),_B), p(g(A),t(A))),
                  [file(auto_variable_name)]).

:- end_tests(auto_variable_name).
