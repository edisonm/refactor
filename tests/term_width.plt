:- begin_tests(term_width).

:- include(refactor_common).

/* $term_width1$
diff -ruN term_width.pl -
--- term_width.pl (source)
+++ term_width.pl (target)
@@ -1,4 +1,7 @@
 :- module(q, [p/1]).
 
 p(a) :-
-    q(aaa, bbb, ccc, ddd, eee, fff, ggg, hhh, iii, jjj, kkk, lll, mmm, nnn, ooo, ppp, qqq).
+    q(aaa, bbb, ccc, ddd, eee,
+      fff, ggg, hhh, iii, jjj,
+      kkk, lll, mmm, nnn, ooo,
+      ppp, qqq, 1).
*/

test(term_width1) :-
    execute_test(term_width1,
                 replace_term(
                     q(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),
                     q(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,1)
                 ),
                [file(term_width), term_width(30)]).

/* $term_width2$
diff -ruN term_width.pl -
--- term_width.pl (source)
+++ term_width.pl (target)
@@ -1,4 +1,8 @@
 :- module(q, [p/1]).
 
 p(a) :-
-    q(aaa, bbb, ccc, ddd, eee, fff, ggg, hhh, iii, jjj, kkk, lll, mmm, nnn, ooo, ppp, qqq).
+    q(aaa, bbb, ccc, ddd,
+      eee, fff, ggg, hhh,
+      iii, jjj, kkk, lll,
+      mmm, nnn, ooo, ppp,
+      qqq, 1).
*/

test(term_width2) :-
    execute_test(term_width2,
                 replace_term(
                     q(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),
                     q(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,1)
                 ),
                [file(term_width), term_width(29)]).

:- end_tests(term_width).
