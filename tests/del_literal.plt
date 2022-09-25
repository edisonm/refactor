:- begin_tests(del_literal).

:- include(refactor_common).

:- use_module(del_literal).

/* $del_literal$
diff -ruN del_literal.pl -
--- del_literal.pl (source)
+++ del_literal.pl (target)
@@ -1,12 +1,10 @@
 :- module(del_literal, [del_literal/0]).
 
 del_literal :-
-    ( a  ),
     b.
 
 del_literal :-
     a,
-    a,
     b.
 
 a.
*/

test(del_literal) :-
    execute_test(del_literal, replace_term((a, b), b)).

:- end_tests(del_literal).
