:- begin_tests(var2term).

:- include(refactor_common).

:- use_module(var2term).

/* $var2term$
diff -ruN var2term.pl -
--- var2term.pl (source)
+++ var2term.pl (target)
@@ -1,3 +1,3 @@
 :- module(var2term, [var2term/2]).
 
-var2term(_A, b).
+var2term_(f(a), b).
*/

test(var2term) :-
    execute_test(var2term, replace_sentence(var2term(_A, B), var2term_(X, B), (X=f(a)))).

:- end_tests(var2term).
