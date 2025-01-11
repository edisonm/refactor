:- begin_tests(sub_lists).

:- include(refactor_common).

:- use_module(sub_lists).

/* $sub_lists$
diff -ruN sub_lists.pl -
--- sub_lists.pl (source)
+++ sub_lists.pl (target)
@@ -1,3 +1,3 @@
 :- module(sub_lists, [t/1]).
 
-t([[a,1,2],[b,3,4],[c,5,6]]).
+t([[a|f(1, 2)]|f([b|f(3, 4)], [c|f(5, 6)])]).
*/

test(sub_lists) :-
    execute_test(sub_lists, sub_lists, replace_term([A,B], f(A,B)), []).

:- end_tests(sub_lists).
