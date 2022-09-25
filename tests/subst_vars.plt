:- begin_tests(subst_vars).

:- include(refactor_common).

:- use_module(subst_vars).

/* $subst_vars_1$
diff -ruN subst_vars.pl -
--- subst_vars.pl (source)
+++ subst_vars.pl (target)
@@ -1,24 +1,19 @@
 :- module(subst_vars, [subst_vars/2]).
 
-subst_vars([A, B], _C) :-
-    A = f(B),
+subst_vars([f(B), B], _C) :-
     true.
 
-subst_vars((A, B), _C) :-
-    A = B,
+subst_vars((B, B), _C) :-
     true.
 
-subst_vars(A, B) :-
-    A = f([/**/B, _C]),
+subst_vars(f([/**/B, _C]), B) :-
     true.
 
-subst_vars(A, B) :-
-    f(A, 'b') = f(a, B),
-    \+ A,
-    \+ B.
+subst_vars(a, 'b') :-
+    \+ a,
+    \+ 'b'.
 
-subst_vars(A, B) :-
-    B = [x|T],
+subst_vars(A, [x|T]) :-
     subst_vars(A, T).
 
 a.
*/

test(subst_vars_1) :-
    execute_test(subst_vars, subst_vars_1,
                 replace_sentence((Head :- A=B, Body),
                                  (Head1$@Head :- Body1$@Body),
                                  ( unifiable(A, B, L1),
                                    maplist(fix_order(Head-Body), L1, L),
                                    substitute_values(L,Head-Body,Head1-Body1))
                                 ), []).

fix_order(Term, A=B, Eq) :-
    ( occurrences_of_var(A, Term, N),
      N > 0
    ->Eq = (A = B)
    ; Eq = (B = A)
    ).

/* $subst_vars_2$
diff -ruN subst_vars.pl -
--- subst_vars.pl (source)
+++ subst_vars.pl (target)
@@ -1,15 +1,12 @@
 :- module(subst_vars, [subst_vars/2]).
 
-subst_vars([A, B], _C) :-
-    A = f(B),
+subst_vars([g(f(B)), B], _C) :-
     true.
 
-subst_vars((A, B), _C) :-
-    A = B,
+subst_vars((g(B), B), _C) :-
     true.
 
-subst_vars(A, B) :-
-    A = f([/**/B, _C]),
+subst_vars(g(f([/**/B, _C])), B) :-
     true.
 
 subst_vars(A, B) :-
@@ -17,8 +14,7 @@
     \+ A,
     \+ B.
 
-subst_vars(A, B) :-
-    B = [x|T],
+subst_vars(A, g([x|T])) :-
     subst_vars(A, T).
 
 a.
*/

test(subst_vars_2) :-
    execute_test(subst_vars, subst_vars_2,
                 replace_sentence((Head :- A=X, Body), (Head :- Body), (A=g(X))), []).

:- end_tests(subst_vars).
