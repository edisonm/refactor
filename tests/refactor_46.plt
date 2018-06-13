:- begin_tests(refactor_46).

:- include(refactor_common).

/* $list1$
diff -ruN list1.pl -
--- list1.pl (source)
+++ list1.pl (target)
@@ -1,2 +1,2 @@
-p([[A, B]]) :-
-  A = [B].
+p([[[B], B]]) :-
+  true.
*/
test(list1) :-
    execute_test(list1,
                  replace_sentence((H1 :- B1), (H2 :- B2),
                                   ( B1 = (A=B),
                                     occurrences_of_var(A, (H1 :- B1), 2),
                                     substitute_values([(A=B)=true, A=B],
                                                       (H1:-B1),(H2:-B2)))),
                  [file(list1)]).

:- end_tests(refactor_46).
