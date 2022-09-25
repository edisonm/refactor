:- begin_tests(keep_list_layout).

:- include(refactor_common).

:- use_module(keep_list_layout).

/* $keep_list_layout$
diff -ruN keep_list_layout.pl -
--- keep_list_layout.pl (source)
+++ keep_list_layout.pl (target)
@@ -1,6 +1,6 @@
 :- module(keep_list_layout, [keep_list_layout/2]).
 
-keep_list_layout([A|_B],A).
-keep_list_layout([A,_B],A).
-keep_list_layout({A,_B},A).
-keep_list_layout((A,_B),A).
+keep_list_layout([a|_B]).
+keep_list_layout([a,_B]).
+keep_list_layout({a,_B}).
+keep_list_layout((a,_B)).
*/

test(keep_list_layout) :-
    execute_test(keep_list_layout, replace_sentence(keep_list_layout(L,A), [keep_list_layout(L1$@L)], (substitute_value(A, a, L, L1)))).

:- end_tests(keep_list_layout).
