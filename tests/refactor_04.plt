:- begin_tests(refactor_04).

:- include(refactor_common).

:- use_module(ex4).

/* $ex4$
diff -ruN ex4.pl -
--- ex4.pl (source)
+++ ex4.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex4, [ex4/2]).
 
-ex4(_A, b).
+ex4_(f(a), b).
*/

test(ex4) :-
    execute_test(ex4, replace_sentence(ex4(_A, B), ex4_(X, B), (X=f(a)))).

:- end_tests(refactor_04).
