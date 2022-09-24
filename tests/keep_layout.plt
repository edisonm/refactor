:- begin_tests(keep_layout).

:- include(refactor_common).

:- use_module(keep_layout).

/* $keep_layout$
diff -ruN keep_layout.pl -
--- keep_layout.pl (source)
+++ keep_layout.pl (target)
@@ -1,6 +1,6 @@
 :- module(keep_layout, [g/0]).
 
-g :- same_term(c,a),d,(b   )   .
+g :- d,(b   )   .
 
 b.
 
*/

test(keep_layout) :-
    execute_test(keep_layout, replace_term((((same_term(c,a),d,b))),(((d,b))))).

:- end_tests(keep_layout).
