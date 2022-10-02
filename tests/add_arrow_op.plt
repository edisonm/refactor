:- begin_tests(add_arrow_op).

:- include(refactor_common).

:- use_module(add_arrow_op).

/* $add_arrow_op$
diff -ruN add_arrow_op.pl -
--- add_arrow_op.pl (source)
+++ add_arrow_op.pl (target)
@@ -1,9 +1,9 @@
 :- module(add_arrow_op, [add_arrow_op/2]).
 
 'add_arrow_op'(A, B) :-
-    once(( A=1,
+    once(( A=1->
            B=a
-         ; A=2,
+         ; A=2->
            B=b
          ; A=3,
            B=c
*/
test(add_arrow_op) :-
    execute_test(add_arrow_op, add_arrow_op,
                 replace_term(((A,B);C), (A->B;C), true), []).

:- end_tests(add_arrow_op).
