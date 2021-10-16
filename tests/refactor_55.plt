:- begin_tests(refactor_55).

:- include(refactor_common).

/* $emptylist$
diff -ruN emptylist.pl -
--- emptylist.pl (source)
+++ emptylist.pl (target)
@@ -1,2 +1,2 @@
-f([], [P]).
-f([], [P|[]]).
+f([], [x(P)]).
+f([], [x(P)|[]]).
*/

test(emptylist) :-
    execute_test(emptylist, replace_term(f(A,B),f(A,C),(B=[M], C=[x(M)])),[file(emptylist)]).

:- end_tests(refactor_55).
