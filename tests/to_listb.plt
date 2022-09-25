:- begin_tests(to_listb).

:- include(refactor_common).

/* $to_listb$
diff -ruN to_listb.pl -
--- to_listb.pl (source)
+++ to_listb.pl (target)
@@ -1 +1,2 @@
-f([a- ~t, b]).
+f([ a- ~t,
+    b]).
*/

test(to_listb) :-
    execute_test(to_listb, replace_term(f(A),f('$LISTB,NL'(A))),[file(to_listb)]).

:- end_tests(to_listb).
