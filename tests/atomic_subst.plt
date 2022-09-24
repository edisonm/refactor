:- begin_tests(atomic_subst).

:- include(refactor_common).

% not aded since it is too noisy:
% test(indent) :-
%     execute_test(indent, replace_sentence((A:-B), '$CLAUSE'(C),
%                                           ( duplicate_term((A:-B),C),
%                                             C=(A:-B)
%                                           )),
%                  [file('refactor.plt')]).

/* $atomic_subst1$
diff -ruN atomic_subst.pl -
--- atomic_subst.pl (source)
+++ atomic_subst.pl (target)
@@ -1,4 +1,3 @@
 
 a :-
-    t(T/*0*/),
-    l([T/*1*/, /*2*/ e(n)]).
+    l([[t/n]/*0*/, /*2*/ e(n)]).
*/

test(atomic_subst1) :-
    execute_test(atomic_subst1,
                 replace_term((t(T), B), C$@B,
                              ( B = l([_, e(X)]),
                                substitute_value(T, [t/X]@@T, B, C)
                              )), [file(atomic_subst)]).

/* $atomic_subst2$
diff -ruN atomic_subst.pl -
--- atomic_subst.pl (source)
+++ atomic_subst.pl (target)
@@ -1,4 +1,3 @@
 
 a :-
-    t(T/*0*/),
-    l([T/*1*/, /*2*/ e(n)]).
+    l([[t/n]/*1*/, /*2*/ e(n)]).
*/

test(atomic_subst2) :-
    execute_test(atomic_subst2,
                 replace_term((t(_), l([Z, e(X)])),
                              l([[t/X]@@Z, e(X)])$@l([Z, e(X)]),
                              true), [file(atomic_subst)]).

:- end_tests(atomic_subst).
