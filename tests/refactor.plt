:- begin_tests(refactor).

:- use_module(library(refactor)).

:- use_module(library(comment_data)).

:- comment_data:enable.

/* $ex1$
--- ex1.pl (source)
+++ ex1.pl (target)
@@ -1,6 +1,6 @@
 :- module(ex1, [g/0]).
 
-g :- same_term(c,a),d,(b   )   .
+g :- d,(b   )   .
 
 b.
 
*/

test(ex1) :-
    [ex1],
    rreset,
    with_output_to(string(Result),
		   replace_term((((same_term(c,a),d,b))),(((d,b))), [module(ex1)])),
    comment_data(ex1, Pattern),
    assertion(Pattern == Result).

/* $ex2$
--- ex2.pl (source)
+++ ex2.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex2, [f/3]).
 
-f(a, f(/*1*/f( a)), f(/*2*/f( f(a  )))).
+g(f(/*2*/f( f(a  ))), f(/*1*/f( a)), a).
*/

test(ex2) :-
    [ex2],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(f(a,f(f(a)),C), g(C,f(f(a)),a),true, [module(ex2)])),
    comment_data(ex2, Pattern),
    assertion(Pattern == Result).

/* $ex3$
--- ex3.pl (source)
+++ ex3.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex3, ['ex3'/0]).
 
-ex3 :- display('ex3').
+ex3 :- ex3, 'ex3', display('ex3').
*/

test(ex3) :-
    [ex3],
    rreset,
    with_output_to(string(Result),
		   expand_sentence((A :- display(B)),
				   (A :- A, B, display(B)), true, [module(ex3)])),
    comment_data(ex3, Pattern),
    assertion(Pattern == Result).

/* $ex4$
--- ex4.pl (source)
+++ ex4.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex4, [ex4/2]).
 
-ex4(_A, b).
+ex4_(f(a), b).
*/

test(ex4) :-
    [ex4],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex4(A, B), ex4_(A, B), (A=f(a)), [module(ex4)])),
    comment_data(ex4, Pattern),
    assertion(Pattern == Result).    

/* $ex5$
--- ex5.pl (source)
+++ ex5.pl (target)
@@ -1,7 +1,7 @@
 :- module(ex5, [ex5/1]).
 
-ex5([]).
-ex5([/* hello */]).
-ex5([d]).
-ex5([d,e]).
-ex5(a).
+ex5([c]).
+ex5([c/* hello */]).
+ex5([c, d]).
+ex5([c, d,e]).
+ex5([c|a]).
*/

test(ex5) :-
    [ex5],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex5(T), ex5([c|T]), true, [module(ex5)])),
    comment_data(ex5, Pattern),
    assertion(Pattern == Result).

/* $ex6$
--- ex6.pl (source)
+++ ex6.pl (target)
@@ -1,7 +1,7 @@
 :- module(ex6, [q/3]).
 
 q(A, B, L) :-
-    p(A, B, L, []).
+    p(B, A, L, []).
 
 p(_, _) --> [].
-p(A, B) --> p(A, B), "hello".
+p(A, B) --> p(B, A), "hello".
*/

test(ex6) :-
    [ex6],
    rreset,
    with_output_to(string(Result),
		   replace_goal(p(A,B,L,T), p(B,A,L,T), [module(ex6)])),
    comment_data(ex6, Pattern),
    assertion(Pattern == Result).

/* $ex7_1$
--- ex7.pl (source)
+++ ex7.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex7, [aaa/3]).
 
-aaa([[d, _]], [/**/ d /* d */], []).
+aab([[_]], e, [d], [[c, /**/ d /* d */], [b, c, /**/ d /* d */]]).
*/

test(ex7_1) :-
    [ex7],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(aaa([[X,_]],[d], []),
				   aab([['$VAR'('_')]], e, [X], [[c,d],[b,c,d]]), true, [module(ex7)])),
    comment_data(ex7_1, Pattern),
    assertion(Pattern == Result).

% Note the difference with previous test, the layout of [d] is
% preserved, due to is exactly the same term, although is via the
% variable X
/* $ex7_2$
--- ex7.pl (source)
+++ ex7.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex7, [aaa/3]).
 
-aaa([[d, _]], [/**/ d /* d */], []).
+aaa([[_]], [d], [[c, /**/ d /* d */], [b, c, /**/ d /* d */]]).
*/

test(ex7_2) :-
    [ex7],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(aaa([[X,_]],[d], []),
				   aaa([['$VAR'('_')]], [X], [[c,d],[b,c,d]]), true, [module(ex7)])),
    comment_data(ex7_2, Pattern),
    assertion(Pattern == Result).

/* $ex8$
--- ex8.pl (source)
+++ ex8.pl (target)
@@ -1,5 +1,5 @@
 :- module(ex8, [ex8/1]).
 
-ex8([[a,b],[c,d],[e]]).
+ex8([[a,b], [e]]).
 
-ex8([[a,b],[c,d]]).
+ex8([[a,b]]).
*/

test(ex8) :-
    [ex8],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex8([[a,b],[c,d]|T]), ex8([[a,b]|T]),
				   true, [module(ex8)])),
    comment_data(ex8, Pattern),
    assertion(Pattern == Result).

/* $ex9$
--- ex9.pl (source)
+++ ex9.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex9, [ex9/2]).
 
-ex9(a, [f(g,c), g(d, e)]).
+ex9(a, [f(g, c, a), g(d, e)]).
*/

test(ex9) :-
    [ex9],
    rreset,
    with_output_to(string(Result),
		   expand_term(f(A,B), f(A,B,X), true,
			       [sentence(ex9(X, _)), module(ex9)])),
    comment_data(ex9, Pattern),
    assertion(Pattern == Result).

/* $ex10_1$
--- ex10.pl (source)
+++ ex10.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex10, [ex10/2]).
 
-ex10(f(A), g(A)).
+ex10(f(A), g(C, a(C))).
*/

test(ex10_1) :-
    [ex10],
    rreset,
    with_output_to(string(Result),
		   expand_term(g(A), g(B,A), ((A=a(B),B='$VAR'('C'))),
			       [sentence(ex10(_, _)), module(ex10)])),
    comment_data(ex10_1, Pattern),
    assertion(Pattern == Result).

/* $ex10_2$
--- ex10.pl (source)
+++ ex10.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex10, [ex10/2]).
 
-ex10(f(A), g(A)).
+ex10(f(A), g(A, f(A))).
*/

test(ex10_2) :-
    [ex10],
    rreset,
    with_output_to(string(Result),
		   expand_term(g(A), g(A,X), true,
			       [sentence(ex10(X, _)), module(ex10 )])),
    comment_data(ex10_2, Pattern),
    assertion(Pattern == Result).

/* $ex11$
--- ex11.pl (source)
+++ ex11.pl (target)
@@ -1,5 +1,5 @@
 :- module(ex11, [ex11/1]).
 
 ex11([A|B]) :-
-    ex11(A),
+    ex11_one(A),
     ex11(B).
*/

test(ex11) :-
    [ex11],
    rreset,
    with_output_to(string(Result),
		   expand_term(ex11(A), ex11_one(A), true,
			       [sentence((ex11([A|_]):-_)),
				module(ex11)])),
    comment_data(ex11, Pattern),
    assertion(Pattern == Result).

/* $ex12$
--- ex12.pl (source)
+++ ex12.pl (target)
@@ -1,12 +1,10 @@
 :- module(ex12, [ex12/0]).
 
 ex12 :-
-    ( a  ),
     b.
 
 ex12 :-
     a,
-    a,
     b.
 
 a.
*/

test(ex12) :-
    [ex12],
    rreset,
    with_output_to(string(Result),
		   replace_term((a, b), b, [module(ex12)])),
    comment_data(ex12, Pattern),
    assertion(Pattern == Result).

/* $ex13$
--- ex13.pl (source)
+++ ex13.pl (target)
@@ -2,7 +2,7 @@
 
 ex13(A, B) :-
     p(A, A),
-    q(B,A),
+    q(B,a),
     r(B, B).
 
 q(1,1).
*/

test(ex13) :-
    [ex13],
    rreset,
    with_output_to(string(Result),
		   expand_term(T, T, (nonvar(T), T=q(_B,A),A=a), [module(ex13)])),
    comment_data(ex13, Pattern),
    assertion(Pattern == Result).

/* $ex14_1$
--- ex14.pl (source)
+++ ex14.pl (target)
@@ -1,24 +1,19 @@
 :- module(ex14, [ex14/2]).
 
-ex14([A, B], _C) :-
-    A = f(B),
+ex14([f(B), B], _C) :-
     true.
 
-ex14((A, B), _C) :-
-    A = B,
+ex14((A, A), _C) :-
     true.
 
-ex14(A, B) :-
-    A = f([/**/B, _C]),
+ex14(f([/**/B, _C]), B) :-
     true.
 
-ex14(A, B) :-
-    f(A, 'b') = f(a, B),
-    \+ A,
-    \+ B.
+ex14(a, 'b') :-
+    \+ a,
+    \+ 'b'.
 
-ex14(A, B) :-
-    B = [x|T],
+ex14(A, [x|T]) :-
     ex14(A, T).
 
 a.
*/

test(ex14_1) :-
    [ex14],
    rreset,
    with_output_to(string(Result),
		   expand_sentence((Head :- A=B, Body), (Head :- Body), (A=B),[module(ex14)])),
    comment_data(ex14_1, Pattern),
    assertion(Pattern == Result).

/* $ex14_2$
--- ex14.pl (source)
+++ ex14.pl (target)
@@ -1,15 +1,12 @@
 :- module(ex14, [ex14/2]).
 
-ex14([A, B], _C) :-
-    A = f(B),
+ex14([g(f(B)), B], _C) :-
     true.
 
-ex14((A, B), _C) :-
-    A = B,
+ex14((g(B), B), _C) :-
     true.
 
-ex14(A, B) :-
-    A = f([/**/B, _C]),
+ex14(g(f([/**/B, _C])), B) :-
     true.
 
 ex14(A, B) :-
@@ -17,8 +14,7 @@
     \+ A,
     \+ B.
 
-ex14(A, B) :-
-    B = [x|T],
+ex14(A, g([x|T])) :-
     ex14(A, T).
 
 a.
*/

test(ex14_2) :-
    [ex14],
    rreset,
    with_output_to(string(Result),
		   expand_sentence((Head :- A=B, Body), (Head :- Body), (A=g(B)),[module(ex14)])),
    comment_data(ex14_2, Pattern),
    assertion(Pattern == Result).

/* $ex15$
--- ex15.pl (source)
+++ ex15.pl (target)
@@ -1,6 +1,6 @@
 :- module(ex15, [ex15/2]).
 
-ex15([A|_B],A).
-ex15([A,_B],A).
-ex15({A,_B},A).
-ex15((A,_B),A).
+ex15([a|_B]).
+ex15([a,_B]).
+ex15({a,_B}).
+ex15((a,_B)).
*/
test(ex15) :-
    [ex15],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex15(L,A), [ex15(L)], (A=a), [module(ex15)])),
    comment_data(ex15, Pattern),
    assertion(Pattern == Result).

/* $ex16$
*/
test(ex16) :-
    [ex16],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(H, H, true, [module(ex16)])),
    comment_data(ex16, Pattern),
    assertion(Pattern == Result).

/* $ex17$
--- ex17.pl (source)
+++ ex17.pl (target)
@@ -1,8 +1,7 @@
 :- module(ex17, [ex17/0]).
 
 ex17 :-
-    a,
-    ( b  ).
+    a.
 
 a.
 
*/
test(ex17) :-
    [ex17],
    rreset,
    with_output_to(string(Result),
		   replace_sentence((H:-(A,_B)), (H:-A), [module(ex17)])),
    comment_data(ex17, Pattern),
    assertion(Pattern == Result).

/* $ex18$
--- ex18.pl (source)
+++ ex18.pl (target)
@@ -1,7 +1,6 @@
 :- module(ex18, [ex18/1]).
 
-ex18(C) :-
-    C=M : H,
+ex18(M : H) :-
     p(M:H).
 
 p(_C).
*/
test(ex18) :-
    [ex18],
    rreset,
    with_output_to(string(Result),
		   expand_sentence((H:-A=B,p(C)), (H:-p(C)), A=B, [module(ex18)])),
    comment_data(ex18, Pattern),
    assertion(Pattern == Result).

/* $ex19_1$
--- ex19.pl (source)
+++ ex19.pl (target)
@@ -1,6 +1,6 @@
 :- module(ex19, [ex19/2, ex19/3]).
 
-ex19(_C, (2,3)).
+ex19((2,3), (2,3)).
 
 ex19(f(/*1*/A, 0 ), _B, f(/*2*/b, f(/*3*/A, 0 ))).
 
*/

test(ex19_1) :-
    [ex19],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex19(C,D), ex19(C,D), C=D, [module(ex19)])),
    comment_data(ex19_1, Pattern),
    assertion(Pattern == Result).

/* $ex19_2$
--- ex19.pl (source)
+++ ex19.pl (target)
@@ -2,8 +2,8 @@
 
 ex19(_C, (2,3)).
 
-ex19(f(/*1*/A, 0 ), _B, f(/*2*/b, f(/*3*/A, 0 ))).
+ex19(f(/*1*/A, 0 ), f(/*2*/b, f(/*3*/A, 0 ))).
 
-ex19([1|C], C, [2,3]).
+ex19([1|[2,3]], [2,3]).
 
-ex19([/*1*/A], _B, [/**/b, A]).
+ex19([/*1*/A], [/**/b, A]).
*/

test(ex19_2) :-
    [ex19],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex19(A,B,C), ex19(A, B), B=C, [module(ex19)])),
    comment_data(ex19_2, Pattern),
    assertion(Pattern == Result).

/* $conjex$
--- conjex.pl (source)
+++ conjex.pl (target)
@@ -1,13 +1,11 @@
 :- module(conjex, [conjex/0]).
 
 conjex :-
-    a(C),
-    b(b),
+    c(C-b),
     c(C),
     d(d).
 conjex :-
-    a(a),
-    b(b).
+    c(a-b).
 
 a(_).
 b(_).
*/

test(conjex) :-
    [conjex],
    rreset,
    with_output_to(string(Result),
		   replace_conjunction(((a(A),b(B))), c(A-B), [module(conjex)])),
    comment_data(conjex, Pattern),
    assertion(Pattern == Result).

/* $two_changes_1$
--- conjex.pl (source)
+++ conjex.pl (target)
@@ -1,15 +1,15 @@
 :- module(conjex, [conjex/0]).
 
 conjex :-
-    a(C),
+    aa(C),
     b(b),
     c(C),
     d(d).
 conjex :-
-    a(a),
+    aa(a),
     b(b).
 
-a(_).
+aa(_).
 b(_).
 c(_).
 d(_).
*/
/* $two_changes_2$
--- conjex.pl (source)
+++ conjex.pl (target)
@@ -6,7 +6,7 @@
     c(C),
     d(d).
 conjex :-
-    aa(a),
+    aa(b),
     b(b).
 
 aa(_).
*/
/* $two_changes_12$
--- conjex.pl (source)
+++ conjex.pl (target)
@@ -1,15 +1,15 @@
 :- module(conjex, [conjex/0]).
 
 conjex :-
-    a(C),
+    aa(C),
     b(b),
     c(C),
     d(d).
 conjex :-
-    a(a),
+    aa(b),
     b(b).
 
-a(_).
+aa(_).
 b(_).
 c(_).
 d(_).
*/

test(two_changes) :-
    [conjex],
    rreset,
    with_output_to(string(Result1), replace_term(a(B),aa(B),[module(conjex)])),
    with_output_to(string(ResultD), rdiff),
    assertion(ResultD == Result1),
    comment_data(two_changes_1, Pattern1),
    assertion(Pattern1 == Result1),
    with_output_to(string(Result2),
		   replace_term(aa(a),aa(b), [module(conjex)])),
    comment_data(two_changes_2, Pattern2),
    assertion(Pattern2 == Result2),
    with_output_to(string(Result12), rshow),
    comment_data(two_changes_12, Pattern12),
    assertion(Pattern12 == Result12),
    once(rundo),
    with_output_to(string(Result3), rshow),
    assertion(Result3==ResultD),
    rsave('/tmp/two_changes.diff'),
    delete_file('/tmp/two_changes.diff').

/* $ex21$
--- ex21.pl (source)
+++ ex21.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex21, [ex21/1]).
 
-ex21(f(b,c,   _D)).
+ex21(g(a,c,   _D)).
*/

test(ex21) :-
    [ex21],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex21(X),ex21(Y),
				   ((X=f(_A,B,C),Y=g(a,B,C))),[module(ex21)])),
    comment_data(ex21, Pattern),
    assertion(Pattern == Result).

/* $ex22$
--- ex22.pl (source)
+++ ex22.pl (target)
@@ -1,4 +1,3 @@
 :- module(ex22, []).
 
-:- dynamic a/1  .
 
*/

test(ex22) :-
    [ex22],
    rreset,
    with_output_to(string(Result),
		   replace_sentence((:- dynamic _), [], [module(ex22)])),
    comment_data(ex22, Pattern),
    assertion(Pattern == Result).

/* $ex23$
--- ex23.pl (source)
+++ ex23.pl (target)
@@ -1,4 +1,4 @@
 :- module(ex23, [ex23/1]).
 
 ex23(X) :-
-    X is 2+6.
+    X is 2+1*6.
*/

test(ex23) :-
    [ex23],
    rreset,
    with_output_to(string(Result),
		   replace_term(A+B, A+(1*B), [module(ex23)])),
    comment_data(ex23, Pattern),
    assertion(Pattern == Result).

/* $ex24$
--- ex24.pl (source)
+++ ex24.pl (target)
@@ -1,4 +1,4 @@
 :- module(ex24, [ex24/1]).
 
 ex24(A) :-
-    A = /****/ key_components/4+ (/*1*/ ( hidden ), ( kbmask([+, +, -, -]) ) /*2*/).
+    A = /****/ key_components/4+ (help, (/*1*/ ( hidden ), ( kbmask([+, +, -, -]) ) /*2*/)).
*/

test(ex24) :-
    [ex24],
    rreset,
    with_output_to(string(Result),
		   replace_term(A/B+P, A/B+(help,P), [module(ex24)])),
    comment_data(ex24, Pattern),
    assertion(Pattern == Result).

/* $ex26$
--- ex26.pl (source)
+++ ex26.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex26, [ex26/1]).
 
-ex26('$sb'(_, _, _, _, _, '$sb'(_, _, _, _, _, _))).
+ex26_('$sb'(_, _, _, _, _, '$sb'(_, _, _, _, _, _))).
*/

test(ex26) :-
    [ex26],
    rreset,
    with_output_to(string(Result),
		   replace_term(ex26(A),ex26_(A), [module(ex26)])),
    comment_data(ex26, Pattern),
    assertion(Pattern == Result).

/* $ex27$
--- ex27.pl (source)
+++ ex27.pl (target)
@@ -4,8 +4,8 @@
 
 ex27 :-
     % test1
-    X = (5,2), % test2
-    b(X).
+    % test2
+    b((5,2)).
 
 ex27_2 :-
     @@(_A, _B).
*/

test(ex27) :-
    [ex27],
    rreset,
    with_output_to(string(Result),
		   expand_term((A=V,Body),A@@Body,A=V,[module(ex27)])),
    comment_data(ex27, Pattern),
    assertion(Pattern == Result).

/* $ex27_2$
--- ex27.pl (source)
+++ ex27.pl (target)
@@ -7,7 +7,6 @@
     X = (5,2), % test2
     b(X).
 
-ex27_2 :-
-    @@(_A, _B).
+@@(_B, _A).
 
 @@(_, _).
*/

test(ex27_2) :-
    [ex27],
    rreset,
    with_output_to(string(Result),
		   replace_sentence((ex27_2:- @@(A,B)),(\\(@@(B,A))),[module(ex27)])),
    comment_data(ex27_2, Pattern),
    assertion(Pattern == Result).

:- [excomm].

test(excomm_1) :-
    rreset,
    with_output_to(string(Result),
		   replace_term(aaa, bbb, [module(excomm)])),
    assertion(Result == "").

/* $excomm_2$
--- excomm.pl (source)
+++ excomm.pl (target)
@@ -10,8 +10,8 @@
 
 f(b->c;true/*1*/).
 
-p(/*1*/_A/*2*/,/*3*/b/*4*/).
-p( /*1*/ a/*2*/ , /*3*/ _B /*4*/ ).
+p(/*3*/b/*4*/,/*1*/_A/*2*/).
+p( /*3*/ _B /*4*/ , /*1*/ a/*2*/ ).
 
 f(a,(a,b,c)).
 
*/

test(excomm_2) :-
    rreset,
    with_output_to(string(Result),
		   replace_term(p(A,B), p(B,A), [module(excomm)])),
    comment_data(excomm_2, Pattern),
    assertion(Pattern == Result).

/* $excomm_3$
--- excomm.pl (source)
+++ excomm.pl (target)
@@ -13,6 +13,6 @@
 p(/*1*/_A/*2*/,/*3*/b/*4*/).
 p( /*1*/ a/*2*/ , /*3*/ _B /*4*/ ).
 
-f(a,(a,b,c)).
+f(a,(b,c)).
 
 f(a;(a,b,c)).
*/
test(excomm_3) :-
    rreset,
    with_output_to(string(Result),
		   replace_sentence(f(a,(_,Body)),f(a,Body), [module(excomm)])),
    comment_data(excomm_3, Pattern),
    assertion(Pattern == Result).

test(self_refactor_1) :-
    rreset,
    with_output_to(string(Result),
		   replace_term(print_expansion(A, B, C, D, E, F),
				print_expansion_(A, B, C, D, E, F), [module(ref_expand)])),
    assertion(Result \== "").

test(self_refactor_2) :-
    rreset,
    with_output_to(string(Result),
		   replace_term(rportray(A, B), rportray_(A, B), [module(ref_expand)])),
    assertion(Result \== "").

test(save_changes) :-
    copy_file('ex1_.pl', '/tmp/ex1_.pl'),
    ['/tmp/ex1_.pl'],
    rreset,
    with_output_to(string(Result),
		   replace_term((((same_term(c,a),d,b))),(((d,b))), [module(ex1_)])),
    assertion(Result\==""),
    rcommit.

:- comment_data:disable.

:- end_tests(refactor).
