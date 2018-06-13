:- begin_tests(refactor_53).

:- include(refactor_common).

/* $conj1$
diff -ruN conj1.pl -
--- conj1.pl (source)
+++ conj1.pl (target)
@@ -2,7 +2,5 @@
 
 conj1(L, PL, R) :-
     findall([A,B],
-            ( member([A, B, C], L),
-              nth1(P, L, [A,B,C]),
-              \+ member(P, PL)
-            ), R).
+            (nth1(P, L, [A,B,C]),
+              \+ member(P, PL)), R).
*/

test(conj1) :-
    execute_test(conj1,
                 replace_conjunction(
                     ( member(E,L),
                       nth1(P,L,E)
                     ),
                     nth1(P,L,E)
                 ),
                [file(conj1)]).

:- end_tests(refactor_53).
