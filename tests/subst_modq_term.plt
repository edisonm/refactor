:- begin_tests(subst_modq_term).

:- include(refactor_common).

:- use_module(subst_modq_term).

/* $subst_modq_term$
diff -ruN subst_modq_term.pl -
--- subst_modq_term.pl (source)
+++ subst_modq_term.pl (target)
@@ -1,7 +1,6 @@
 :- module(subst_modq_term, [subst_modq_term/1]).
 
-subst_modq_term(C) :-
-    C=M : H,
+subst_modq_term(M : H) :-
     p(M:H).
 
 p(_C).
*/

test(subst_modq_term) :-
    execute_test(subst_modq_term, replace_sentence((H:-A=B,p(C)), (H1:-p(C1)), substitute_value(A,B,H-C,H1-C1))).

:- end_tests(subst_modq_term).
