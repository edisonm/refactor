:- begin_tests(substitute_list).

:- include(refactor_common).

/* $substitute_list$
diff -ruN substitute_list.pl -
--- substitute_list.pl (source)
+++ substitute_list.pl (target)
@@ -1,2 +1,2 @@
-p([[A, B]]) :-
-  A = [B].
+p([[[B], B]]) :-
+  true.
*/
test(substitute_list) :-
    execute_test(substitute_list,
                 replace_sentence((H1 :- B1), (H2 :- B2),
                                  ( B1 = (A=B),
                                    occurrences_of_var(A, (H1 :- B1), 2),
                                    substitute_values([(A=B)=true, A=B],
                                                      (H1:-B1),(H2:-B2)))),
                 [file(substitute_list)]).

:- end_tests(substitute_list).
