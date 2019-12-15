:- begin_tests(refactor_19).

:- include(refactor_common).
:- use_module(library(substitute)).

:- use_module(ex19).

/* $ex19_1$
diff -ruN ex19.pl -
--- ex19.pl (source)
+++ ex19.pl (target)
@@ -1,6 +1,6 @@
 :- module(ex19, [ex19/2, ex19/3]).
 
-ex19(_C, (2,3)).
+ex19((2,3), (2,3)).
 
 ex19(f(/*1*/A, 0 ), _B, f(/*2*/b, f(/*3*/A, 0 ))).
 
*/

test(ex19_1) :-
    execute_test(ex19, ex19_1, replace_sentence(ex19(_C,D), ex19(D,D)), []).

/* $ex19_2$
diff -ruN ex19.pl -
--- ex19.pl (source)
+++ ex19.pl (target)
@@ -2,8 +2,8 @@
 
 ex19(_C, (2,3)).
 
-ex19(f(/*1*/A, 0 ), _B, f(/*2*/b, f(/*3*/A, 0 ))).
+ex19(f(/*1*/A, 0 ), f(/*2*/b, f(/*3*/A, 0 ))).
 
-ex19([1|C], C, [2,3]).
+ex19([1, 2,3], [2,3]).
 
-ex19([/*1*/A], _B, [/**/b, A]).
+ex19([/*1*/A], [/**/b, A]).
*/

test(ex19_2) :-
    execute_test(ex19, ex19_2,
                 replace_sentence(ex19(A, B, C), ex19(AS$@A, C),
                                  substitute_value(B, C, A, AS)), []).

:- end_tests(refactor_19).
