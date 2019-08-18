:- begin_tests(refactor_32).

:- include(refactor_common).

% not aded since it is too noisy:
% test(indent) :-
%     execute_test(indent, replace_sentence((A:-B), '$CLAUSE'(C),
%                                           ( duplicate_term((A:-B),C),
%                                             C=(A:-B)
%                                           )),
%                  [file('refactor.plt')]).

/* $atomic$
diff -ruN atomic.pl -
--- atomic.pl (source)
+++ atomic.pl (target)
@@ -1,4 +1,3 @@
 
 a :-
-    t(T),
-    l([T,e(n)]).
+    l([[t/n],e(n)]).
*/

test(atomic) :-
    execute_test(atomic,
                 replace_term((t(T), B), C$@B,
                              ( B = l([_, e(X)]),
                                substitute_value(T, [t/X], B, C)
                              )), [file(atomic), linearize([atom])]).

:- end_tests(refactor_32).
