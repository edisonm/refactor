:- begin_tests(refactor_32).

:- include(refactor_common).

% not aded since it is too noisy:
% test(indent) :-
%     execute_test(indent, replace_sentence((A:-B), '$CLAUSE'(C),
%                                           ( duplicate_term((A:-B),C),
%                                             C=(A:-B)
%                                           )),
%                  [file('refactor.plt')]).

/* $atomic1$
diff -ruN atomic.pl -
--- atomic.pl (source)
+++ atomic.pl (target)
@@ -1,4 +1,3 @@
 
 a :-
-    t(T/*0*/),
-    l([T/*1*/, /*2*/ e(n)]).
+    l([[t/n]/*0*/, /*2*/ e(n)]).
*/

test(atomic1) :-
    execute_test(atomic1,
                 replace_term((t(T), B), C$@B,
                              ( B = l([_, e(X)]),
                                substitute_value(T, [t/X]@@T, B, C)
                              )), [file(atomic)]).

/* $atomic2$
diff -ruN atomic.pl -
--- atomic.pl (source)
+++ atomic.pl (target)
@@ -1,4 +1,3 @@
 
 a :-
-    t(T/*0*/),
-    l([T/*1*/, /*2*/ e(n)]).
+    l([[t/n]/*1*/, /*2*/ e(n)]).
*/

test(atomic2) :-
    execute_test(atomic2,
                 replace_term((t(_), l([Z, e(X)])),
                              l([[t/X]@@Z, e(X)])$@l([Z, e(X)]),
                              true), [file(atomic)]).

:- end_tests(refactor_32).
