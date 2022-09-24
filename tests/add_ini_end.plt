:- begin_tests(add_ini_end).

:- include(refactor_common).

:- use_module(keep_layout).

/* $add_ini$
diff -ruN keep_layout.pl -
--- keep_layout.pl (source)
+++ keep_layout.pl (target)
@@ -1,3 +1,4 @@
+a.
 :- module(keep_layout, [g/0]).
 
 g :- same_term(c,a),d,(b   )   .
*/

test(add_ini) :-
    execute_test(add_ini, replace_sentence([], [a], true), [file(keep_layout)]).

/* $add_end$
diff -ruN keep_layout.pl -
--- keep_layout.pl (source)
+++ keep_layout.pl (target)
@@ -5,3 +5,4 @@
 b.
 
 d.
+a.
*/

test(add_end) :-
    execute_test(add_end, replace_sentence(end_of_file, [a], true), [file(keep_layout)]).

:- end_tests(add_ini_end).
