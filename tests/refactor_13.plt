:- begin_tests(refactor_13).

:- include(refactor_common).

:- use_module(ex13).

/* $ex13$
diff -ruN ex13.pl -
--- ex13.pl (source)
+++ ex13.pl (target)
@@ -2,7 +2,7 @@
 
 ex13(A, B) :-
     p(A, A),
-    q(B,A),
+    q(B,a),
     r(B, B).
 
 q(1,1).
*/

test(ex13) :-
    once(clause(ex13:ex13(_,_), _, Ref)),
    execute_test(ex13, replace_term(T, T1$@T,
                                    ( nonvar(T),
                                      T=q(_B,A),
                                      var(A),
                                      substitute_value(A, a, T, T1)
                                    )), [clause(Ref)]).

:- end_tests(refactor_13).
