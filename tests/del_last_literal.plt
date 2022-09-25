:- begin_tests(del_last_literal).

:- include(refactor_common).

:- use_module(del_last_literal).

/* $del_last_literal$
diff -ruN del_last_literal.pl -
--- del_last_literal.pl (source)
+++ del_last_literal.pl (target)
@@ -1,8 +1,7 @@
 :- module(del_last_literal, [del_last_literal/0]).
 
 del_last_literal :-
-    a,
-    ( b  ).
+    a.
 
 a.
 
*/

test(del_last_literal) :-
    execute_test(del_last_literal, replace_sentence((H:-(A,_B)), (H:-A))).

:- end_tests(del_last_literal).
