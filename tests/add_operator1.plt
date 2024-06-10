:- begin_tests(add_operator1).

:- include(refactor_common).

:- use_module(add_operator1).

/* $add_operator1$
diff -ruN add_operator1.pl -
--- add_operator1.pl (source)
+++ add_operator1.pl (target)
@@ -3,4 +3,4 @@
             op(980, fx, #)
           ]).
 
-p(~a-b).
+p(# ~a-b).
*/

test(add_operator1) :-
    execute_test(add_operator1, replace_term(p(A), p(#A))).

:- end_tests(add_operator1).
