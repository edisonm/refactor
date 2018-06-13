:- begin_tests(refactor_01).

:- include(refactor_common).

:- use_module(ex1).

/* $ex1$
diff -ruN ex1.pl -
--- ex1.pl (source)
+++ ex1.pl (target)
@@ -1,6 +1,6 @@
 :- module(ex1, [g/0]).
 
-g :- same_term(c,a),d,(b   )   .
+g :- d,(b   )   .
 
 b.
 
*/

test(ex1) :-
    execute_test(ex1, replace_term((((same_term(c,a),d,b))),(((d,b))))).

/* $addini$
diff -ruN ex1.pl -
--- ex1.pl (source)
+++ ex1.pl (target)
@@ -1,3 +1,4 @@
+a.
 :- module(ex1, [g/0]).
 
 g :- same_term(c,a),d,(b   )   .
*/

test(addini) :-
    execute_test(addini, replace_sentence([], [a], true), [file(ex1)]).

/* $addend$
diff -ruN ex1.pl -
--- ex1.pl (source)
+++ ex1.pl (target)
@@ -5,3 +5,4 @@
 b.
 
 d.
+a.
*/

test(addend) :-
    execute_test(addend, replace_sentence(end_of_file, [a], true), [file(ex1)]).

:- end_tests(refactor_01).
