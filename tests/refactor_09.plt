:- begin_tests(refactor).

:- include(refactor_common).

:- use_module(ex9).

/* $ex9$
diff -ruN ex9.pl -
--- ex9.pl (source)
+++ ex9.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex9, [ex9/2]).
 
-ex9(a, [f(g,c), g(d, e)]).
+ex9(a, [f(g, c, a), g(d, e)]).
*/

test(ex9) :-
    execute_test(ex9, ex9, replace_term(f(A,B), f(A,B,X)), [sentence(ex9(X, _))]).

:- end_tests(refactor).
