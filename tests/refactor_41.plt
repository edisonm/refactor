:- begin_tests(refactor).

:- include(refactor_common).

:- use_module(exst).

/* $exst$
diff -ruN exst.pl -
--- exst.pl (source)
+++ exst.pl (target)
@@ -1,3 +1,3 @@
 :- module(exst, [p/1]).
 
-p([a,b,c,d]).
+p([a-"A",b-"B",c-"C",d-"D"]).
*/

test(exst) :-
    execute_test(exst, exst, replace_term(X, X-Y, (atom(X), string_upper(X, Y))), [sentence(p(_))]).

:- end_tests(refactor).
