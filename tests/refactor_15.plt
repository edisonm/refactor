:- begin_tests(refactor_15).

:- include(refactor_common).

:- use_module(ex15).

/* $ex15$
diff -ruN ex15.pl -
--- ex15.pl (source)
+++ ex15.pl (target)
@@ -1,6 +1,6 @@
 :- module(ex15, [ex15/2]).
 
-ex15([A|_B],A).
-ex15([A,_B],A).
-ex15({A,_B},A).
-ex15((A,_B),A).
+ex15([a|_B]).
+ex15([a,_B]).
+ex15({a,_B}).
+ex15((a,_B)).
*/

test(ex15) :-
    execute_test(ex15, replace_sentence(ex15(L,A), [ex15(L1$@L)], (substitute_value(A, a, L, L1)))).

:- end_tests(refactor_15).
