:- begin_tests(add_operator2).

:- include(refactor_common).

:- use_module(add_operator2).

/* $add_operator2$
diff -ruN add_operator2.pl -
--- add_operator2.pl (source)
+++ add_operator2.pl (target)
@@ -3,4 +3,4 @@
             op(980, fx, #)
           ]).
 
-q(~x).
+q(a- ~x).
*/

test(add_operator2) :-
    execute_test(add_operator2, replace_term(q(A), q(a-A))).

:- end_tests(add_operator2).
