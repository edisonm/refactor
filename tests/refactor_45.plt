:- begin_tests(refactor).

:- include(refactor_common).
:- use_module(library(clambda)).

/* $newvars$
diff -ruN newvars.pl -
--- newvars.pl (source)
+++ newvars.pl (target)
@@ -1,2 +1,2 @@
 
-p(a(_N), R) :- R = g(f(a), "b").
+p(N, A1, R) :- R = g(f(N), A1, A2, A2, _, "b").
*/

test(newvars) :-
    execute_test(newvars,
                  \ Options
                 ^( replace_sentence((H1 :- B1), (H2 :- B2),
                                     (H1 = p(a(X), B),
                                      H2 = p(X, Y, B),
                                      B1 = (R = g(_, "b")),
                                      B2 = (R = g(f(X), Y, A, A, _D, "b"))
                                     ), Options),
                    remove_underscore_multi(Options),
                    anonymize_all_singletons(Options)
                  ),
                  [file(newvars), vars_preffix('A')]).

:- end_tests(refactor).
