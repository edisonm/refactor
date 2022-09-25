:- begin_tests(add_singleton).

:- include(refactor_common).

/* $add_singleton$
diff -ruN add_singleton.pl -
--- add_singleton.pl (source)
+++ add_singleton.pl (target)
@@ -1,2 +1,2 @@
 
-f(_, B).
+f(_, B, _).
*/

test(add_singleton) :-
    execute_test(add_singleton, replace_sentence(f(A,B),f(A,B,_)), [file(add_singleton)]).

:- end_tests(add_singleton).
