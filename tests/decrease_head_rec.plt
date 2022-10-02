:- begin_tests(decrease_head_rec).

:- include(refactor_common).

/* $decrease_head_rec$
diff -ruN decrease_head_rec.pl -
--- decrease_head_rec.pl (source)
+++ decrease_head_rec.pl (target)
@@ -1,2 +1,2 @@
 
-p([p(a),f(p(b))]) :- p(X),X=1.
+q([q(a),f(q(b))]) :- p(X),X=1.
*/

test(decrease_head_rec) :-
    execute_test(decrease_head_rec, replace(head_rec, p(A), q(A)), [file(decrease_head_rec)]).

:- end_tests(decrease_head_rec).
