:- begin_tests(subst_layout).

:- include(refactor_common).

:- use_module(subst_layout).

/* $subst_layout$
diff -ruN subst_layout.pl -
--- subst_layout.pl (source)
+++ subst_layout.pl (target)
@@ -2,7 +2,7 @@
 
 subst_layout(A, B) :-
     p(A, A),
-    q(B,A),
+    q(B,a),
     r(B, B).
 
 q(1,1).
*/

test(subst_layout) :-
    once(clause(subst_layout:subst_layout(_,_), _, Ref)),
    execute_test(subst_layout,
                 replace_term(T, T1$@T,
                              ( nonvar(T),
                                T=q(_B,A),
                                var(A),
                                substitute_value(A, a, T, T1)
                              )), [clause(Ref)]).

:- end_tests(subst_layout).
