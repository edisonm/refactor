:- begin_tests(rename_functor).

:- include(refactor_common).

:- use_module(rename_functor).

/* $rename_functor$
diff -ruN rename_functor.pl -
--- rename_functor.pl (source)
+++ rename_functor.pl (target)
@@ -1,3 +1,3 @@
 :- module(rename_functor, [rename_functor/1]).
 
-rename_functor('$sb'(_, _, _, _, _, '$sb'(_, _, _, _, _, _))).
+rename_functor_('$sb'(_, _, _, _, _, '$sb'(_, _, _, _, _, _))).
*/

test(rename_functor) :-
    execute_test(rename_functor, replace_term(rename_functor(A),rename_functor_(A))).

:- end_tests(rename_functor).
