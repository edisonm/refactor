:- begin_tests(add_sentence_var).

:- include(refactor_common).

:- use_module(add_sentence_var).

/* $add_sentence_var$
diff -ruN add_sentence_var.pl -
--- add_sentence_var.pl (source)
+++ add_sentence_var.pl (target)
@@ -1,5 +1,5 @@
 :- module(add_sentence_var, [add_sentence_var/1]).
 
 add_sentence_var([A|B]) :-
-    add_sentence_var(A),
+    add_sentence_var_one(A, B),
     add_sentence_var(B).
*/

test(add_sentence_var) :-
    execute_test(add_sentence_var, add_sentence_var,
                 replace_term(add_sentence_var(A), add_sentence_var_one(A, B), true),
                 [sentence((add_sentence_var([A|B]):-_))]).

:- end_tests(add_sentence_var).
