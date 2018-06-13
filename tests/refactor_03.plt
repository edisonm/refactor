:- begin_tests(refactor_03).

:- include(refactor_common).

:- use_module(ex3).

/* $ex3$
diff -ruN ex3.pl -
--- ex3.pl (source)
+++ ex3.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex3, ['ex3'/0]).
 
-ex3 :- display('ex3').
+ex3 :- ex3, 'ex3', display('ex3').
*/

test(ex3) :-
    execute_test(ex3, replace_sentence((A :- display(B)),
                                       (A :- A, B, display(B)), true)).

:- end_tests(refactor_03).
