:- begin_tests(refactor).

:- include(refactor_common).

:- use_module(fpex).

/* $fpex$
diff -ruN fpex.pl -
--- fpex.pl (source)
+++ fpex.pl (target)
@@ -1,9 +1,9 @@
 :- module(fpex, [fpex/2]).
 
 'fpex'(A, B) :-
-    once(( A=1,
+    once(( A=1->
            B=a
-         ; A=2,
+         ; A=2->
            B=b
          ; A=3,
            B=c
*/
test(fpex) :-
    execute_test(fpex, fpex,
                 replace_term(((A,B);C), (A->B;C), true), []).

:- end_tests(refactor).
