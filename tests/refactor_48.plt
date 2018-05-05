:- begin_tests(refactor).

:- include(refactor_common).

/* $decr$
diff -ruN decr.pl -
--- decr.pl (source)
+++ decr.pl (target)
@@ -1,2 +1,2 @@
 
-p([p(a),f(p(b))]) :- p(X),X=1.
+q([q(a),f(q(b))]) :- p(X),X=1.
*/

test(decr) :-
    execute_test(decr, replace(head_rec, p(A), q(A)), [file(decr)]).

:- end_tests(refactor).
