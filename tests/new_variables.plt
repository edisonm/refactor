:- begin_tests(new_variables).

:- include(refactor_common).
:- use_module(library(clambda)).

/* $new_variables$
diff -ruN new_variables.pl -
--- new_variables.pl (source)
+++ new_variables.pl (target)
@@ -1,2 +1,2 @@
 
-p(a(_N), R) :- R = g(f(a), "b").
+p(N, A1, R) :- R = g(f(N), A1, A2, A2, _, "b").
*/

test(new_variables) :-
    execute_test(new_variables,
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
                  [file(new_variables), vars_prefix('A')]).

:- end_tests(new_variables).
