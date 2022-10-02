:- begin_tests(term_with_op).

:- include(refactor_common).

:- use_module(term_with_op).

/* $term_with_op$
diff -ruN term_with_op.pl -
--- term_with_op.pl (source)
+++ term_with_op.pl (target)
@@ -1,4 +1,4 @@
 :- module(term_with_op, [term_with_op/1]).
 
 term_with_op(A) :-
-    A = /****/ key_components/4+ (/*1*/ ( hidden ), ( kbmask([+, +, -, -]) ) /*2*/).
+    A = /****/ key_components/4+ (/*1*/ ( help ), (/*1*/ ( hidden ), ( kbmask([+, +, -, -]) ) /*2*/) /*2*/).
*/

test(term_with_op) :-
    execute_test(term_with_op, replace_term(A/B+P, A/B+(help,P))).

:- end_tests(term_with_op).
