:- begin_tests(remove_literal).

:- include(refactor_common).

/* $remove_literal$
diff -ruN remove_literal.pl -
--- remove_literal.pl (source)
+++ remove_literal.pl (target)
@@ -2,7 +2,5 @@
 
 remove_literal(L, PL, R) :-
     findall([A,B],
-            ( member([A, B, C], L),
-              nth1(P, L, [A,B,C]),
-              \+ member(P, PL)
-            ), R).
+            (nth1(P, L, [A,B,C]),
+              \+ member(P, PL)), R).
*/

test(remove_literal) :-
    execute_test(remove_literal,
                 replace_conjunction(
                     ( member(E,L),
                       nth1(P,L,E)
                     ),
                     nth1(P,L,E)
                 ),
                [file(remove_literal)]).

:- end_tests(remove_literal).
