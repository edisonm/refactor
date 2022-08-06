:- begin_tests(remove_sentence).

:- include(refactor_common).

:- use_module(remove_sentence).

/* $remove_sentence$
diff -ruN remove_sentence.pl -
--- remove_sentence.pl (source)
+++ remove_sentence.pl (target)
@@ -1,4 +1,2 @@
 :- module(remove_sentence, []).
 
-:- dynamic a/1  .
-
*/

test(remove_sentence) :-
    execute_test(remove_sentence, replace_sentence((:- dynamic _), [])).

:- end_tests(remove_sentence).
