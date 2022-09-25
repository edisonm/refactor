:- begin_tests(comments_ex).

:- include(refactor_common).

:- use_module(comments_ex).

/* $comments_ex_1$
*/

test(comments_ex_1) :-
    execute_test(comments_ex, comments_ex_1, replace_term(aaa, bbb), []).

/* $comments_ex_2$
diff -ruN comments_ex.pl -
--- comments_ex.pl (source)
+++ comments_ex.pl (target)
@@ -10,8 +10,8 @@
    ->%(
    c;b).
 
-p(/*1*/_A/*2*/,/*3*/b/*4*/).
-p( /*1*/ a/*2*/ , /*3*/ _B /*4*/ ).
+p(/*3*/b/*4*/,/*1*/_A/*2*/).
+p( /*3*/ _B /*4*/ , /*1*/ a/*2*/ ).
 
 f(b->c;true/*1*/).
 
*/

test(comments_ex_2) :-
    execute_test(comments_ex, comments_ex_2, replace_term(p(A,B), p(B,A)), []).

/* $comments_ex_3$
diff -ruN comments_ex.pl -
--- comments_ex.pl (source)
+++ comments_ex.pl (target)
@@ -17,4 +17,4 @@
 
 f(a;(a,b,c)).
 
-f(a,(a,b,c)).
+f(a,(b,c)).
*/

test(comments_ex_3) :-
    execute_test(comments_ex, comments_ex_3, replace_sentence(f(a,(_,Body)), f(a,Body)), []).

:- end_tests(comments_ex).
