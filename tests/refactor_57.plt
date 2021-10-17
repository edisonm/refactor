:- begin_tests(refactor_57).

:- include(refactor_common).

/* $reflist$
diff -ruN reflist.pl -
--- reflist.pl (source)
+++ reflist.pl (target)
@@ -1 +1 @@
-f([A|T], B, [/**/]).
+f([], [A|T], [/**/]).
*/

test(reflist) :-
    execute_test(reflist, replace_term(f(X,B,C), f(B,X,C), B=[]),[file(reflist)]).

:- end_tests(refactor_57).
