:- begin_tests(refactor_28).

:- include(refactor_common).

:- use_module(excomm).

/* $excomm_1$
*/

test(excomm_1) :-
    execute_test(excomm, excomm_1, replace_term(aaa, bbb), []).

/* $excomm_2$
diff -ruN excomm.pl -
--- excomm.pl (source)
+++ excomm.pl (target)
@@ -10,8 +10,8 @@
    ->%(
    c;b).
 
-p(/*1*/_A/*2*/,/*3*/b/*4*/).
-p( /*1*/ a/*2*/ , /*3*/ _B /*4*/ ).
+p(/*3*/b/*4*/,/*1*/_A/*2*/).
+p( /*3*/ _B /*4*/ , /*1*/ a/*2*/ ).
 
 f(b->c;true/*1*/).
 
*/

test(excomm_2) :-
    execute_test(excomm, excomm_2, replace_term(p(A,B), p(B,A)), []).

/* $excomm_3$
diff -ruN excomm.pl -
--- excomm.pl (source)
+++ excomm.pl (target)
@@ -17,4 +17,4 @@
 
 f(a;(a,b,c)).
 
-f(a,(a,b,c)).
+f(a,(b,c)).
*/

test(excomm_3) :-
    execute_test(excomm, excomm_3, replace_sentence(f(a,(_,Body)), f(a,Body)), []).

:- end_tests(refactor_28).
