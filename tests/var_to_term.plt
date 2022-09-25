:- begin_tests(var_to_term).

:- include(refactor_common).

:- use_module(var_to_term).

/* $var_to_term$
diff -ruN var_to_term.pl -
--- var_to_term.pl (source)
+++ var_to_term.pl (target)
@@ -1,3 +1,3 @@
 :- module(var_to_term, [var_to_term/2]).
 
-var_to_term(_A, b).
+var_to_term_(f(a), b).
*/

test(var_to_term) :-
    execute_test(var_to_term, replace_sentence(var_to_term(_A, B), var_to_term_(X, B), (X=f(a)))).

:- end_tests(var_to_term).
