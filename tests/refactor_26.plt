:- begin_tests(refactor).

:- include(refactor_common).

:- use_module(ex26).

/* $ex26$
diff -ruN ex26.pl -
--- ex26.pl (source)
+++ ex26.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex26, [ex26/1]).
 
-ex26('$sb'(_, _, _, _, _, '$sb'(_, _, _, _, _, _))).
+ex26_('$sb'(_, _, _, _, _, '$sb'(_, _, _, _, _, _))).
*/

test(ex26) :-
    execute_test(ex26, replace_term(ex26(A),ex26_(A))).

:- end_tests(refactor).
