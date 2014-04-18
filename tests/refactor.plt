:- begin_tests(refactor).

:- use_module(library(refactor)).

:- use_module(comment_data).

:- comment_data:enable.

/* $example_1$
--- ex1.pl (source)
+++ ex1.pl (target)
@@ -1,6 +1,6 @@
 :- module(ex1, [g/0]).
 
-g :- same_term(c,a),d,(b   )   .
+g :- d,(b   )   .
 
 b.
 
*/

test(example_1) :-
    [ex1],
    rreset,
    with_output_to(string(Result),
		   replace_term(ex1:_, (same_term(c,a),d,b),(d,b), [])),
    comment_data(example_1, Pattern),
    assertion(Pattern == Result).

/* $example_2$
--- ex2.pl (source)
+++ ex2.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex2, [f/3]).
 
-f(a, f(/*1*/f( a)), f(/*2*/f( f(a  )))).
+g(f(/*2*/f( f(a  ))), f(/*1*/f( a)), a).
*/

test(example_2) :-
    [ex2],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex2:f(a,f(f(a)),C), g(C,f(f(a)),a),true, [])),
    comment_data(example_2, Pattern),
    assertion(Pattern == Result).

/* $example_3$
--- ex3.pl (source)
+++ ex3.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex3, ['ex3'/0]).
 
-ex3 :- display('ex3').
+ex3 :- ex3, 'ex3', display('ex3').
*/

test(example_3) :-
    [ex3],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex3:(A :- display(B)),
				   (A :- A, B, display(B)), true, [])),
    comment_data(example_3, Pattern),
    assertion(Pattern == Result).

/* $example_4$
--- ex4.pl (source)
+++ ex4.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex4, [ex4/2]).
 
-ex4(_A, b).
+ex4_(f(a), b).
*/

test(example_4) :-
    [ex4],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex4:ex4(A, B), ex4_(A, B), (A=f(a)), [])),
    comment_data(example_4, Pattern),
    assertion(Pattern == Result).    

/* $example_5$
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

test(example_5) :-
    [ex5],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex5:ex5(T), ex5([c|T]), true, [])),
    comment_data(example_5, Pattern),
    assertion(Pattern == Result).

/* $example_6$
--- ex6.pl (source)
+++ ex6.pl (target)
@@ -1,7 +1,7 @@
 :- module(ex6, []).
 
 q(A, B, L) :-
-    p(A, B, L, []).
+    p(B, A, L, []).
 
 p(_, _) --> [].
-p(A, B) --> p(A, B), "hello".
+p(A, B) --> p(B, A), "hello".
*/

test(example_6) :-
    [ex6],
    rreset,
    with_output_to(string(Result),
		   replace_goal(ex6:_, p(A,B,L,T), p(B,A,L,T), [])),
    comment_data(example_6, Pattern),
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
		   expand_sentence(ex7:aaa([[X,_]],[d], []),
				   aab([['$VAR'('_')]], e, [X], [[c,d],[b,c,d]]), true, [])),
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
+aaa([[_]], [/**/ d /* d */], [[c, /**/ d /* d */], [b, c, /**/ d /* d */]]).
*/

test(ex7_2) :-
    [ex7],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex7:aaa([[X,_]],[d], []),
				   aaa([['$VAR'('_')]], [X], [[c,d],[b,c,d]]), true, [])),
    comment_data(ex7_2, Pattern),
    assertion(Pattern == Result).

/* $example_8$
--- ex8.pl (source)
+++ ex8.pl (target)
@@ -1,5 +1,5 @@
 :- module(ex8, [ex8/1]).
 
-ex8([[a,b],[c,d],[e]]).
+ex8([[a,b], [e]]).
 
-ex8([[a,b],[c,d]]).
+ex8([[a,b]]).
*/

test(example_8) :-
    [ex8],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex8:ex8([[a,b],[c,d]|T]), ex8([[a,b]|T]),
				   true, [])),
    comment_data(example_8, Pattern),
    assertion(Pattern == Result).

/* $example_9$
--- ex9.pl (source)
+++ ex9.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex9, [ex9/2]).
 
-ex9(a, [f(g,c), g(d, e)]).
+ex9(a, [f(g, c, a), g(d, e)]).
*/

test(example_9) :-
    [ex9],
    rreset,
    with_output_to(string(Result),
		   expand_term(ex9:ex9(X, _), f(A,B), f(A,B,X), true, [])),
    comment_data(example_9, Pattern),
    assertion(Pattern == Result).

/* $example_10_1$
--- ex10.pl (source)
+++ ex10.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex10, [ex10/2]).
 
-ex10(f(A), g(A)).
+ex10(f(A), g(C, a(C))).
*/

test(example_10_1) :-
    [ex10],
    rreset,
    with_output_to(string(Result),
		   expand_term(ex10:ex10(_, _), g(A), g(B,A), (A=a(B),B='$VAR'('C')), [])),
    comment_data(example_10_1, Pattern),
    assertion(Pattern == Result).

/* $example_10_2$
--- ex10.pl (source)
+++ ex10.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex10, [ex10/2]).
 
-ex10(f(A), g(A)).
+ex10(f(A), g(A, f(A))).
*/

test(example_10_2) :-
    [ex10],
    rreset,
    with_output_to(string(Result),
		   expand_term(ex10:ex10(X, _), g(A), g(A,X), true, [])),
    comment_data(example_10_2, Pattern),
    assertion(Pattern == Result).

/* $example_11$
--- ex11.pl (source)
+++ ex11.pl (target)
@@ -1,5 +1,5 @@
 :- module(ex11, [ex11/1]).
 
 ex11([A|B]) :-
-    ex11(A),
+    ex11_one(A),
     ex11(B).
*/

test(example_11) :-
    [ex11],
    rreset,
    with_output_to(string(Result),
		   expand_term(ex11:(ex11([A|_]):-_), ex11(A), ex11_one(A), true, [])),
    comment_data(example_11, Pattern),
    assertion(Pattern == Result).

/* $example_12$
--- ex12.pl (source)
+++ ex12.pl (target)
@@ -1,12 +1,10 @@
 :- module(ex12, [ex12/0]).
 
 ex12 :-
-    ( a  ),
-    b.
+    ( b.
 
 ex12 :-
     a,
-    a,
     b.
 
 a.
*/

test(example_12) :-
    [ex12],
    rreset,
    with_output_to(string(Result),
		   replace_term(ex12:_, (a, b), b, [])),
    comment_data(example_12, Pattern),
    assertion(Pattern == Result).

/* $example_13$
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

test(example_13) :-
    [ex13],
    rreset,
    with_output_to(string(Result),
		   expand_term(ex13:_, T, T, (nonvar(T), T=q(_B,A),A=a), [])),
    comment_data(example_13, Pattern),
    assertion(Pattern == Result).

/* $example_14_1$
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

test(example_14_1) :-
    [ex14],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex14:(Head :- A=B, Body), (Head :- Body), (A=B),[])),
    comment_data(example_14_1, Pattern),
    assertion(Pattern == Result).

/* $example_14_2$
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

test(example_14_2) :-
    [ex14],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex14:(Head :- A=B, Body), (Head :- Body), (A=g(B)),[])),
    comment_data(example_14_2, Pattern),
    assertion(Pattern == Result).

/* $example_15$
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
test(example_15) :-
    [ex15],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex15:ex15(L,A), ex15(L), (A=a), [])),
    comment_data(example_15, Pattern),
    assertion(Pattern == Result).

/* $example_16$
*/
test(example_16) :-
    [ex16],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex16:H, H, true, [])),
    comment_data(example_16, Pattern),
    assertion(Pattern == Result).

/* $example_17$
--- ex17.pl (source)
+++ ex17.pl (target)
@@ -1,8 +1,7 @@
 :- module(ex17, [ex17/0]).
 
 ex17 :-
-    a,
-    ( b  ).
+    a  ).
 
 a.
 
*/
test(example_17) :-
    [ex17],
    rreset,
    with_output_to(string(Result),
		   replace_sentence(ex17:(H:-(A,_B)), (H:-A), [])),
    comment_data(example_17, Pattern),
    assertion(Pattern == Result).

/* $example_18$
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
test(example_18) :-
    [ex18],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex18:(H:-A=B,p(C)), (H:-p(C)), A=B, [])),
    comment_data(example_18, Pattern),
    assertion(Pattern == Result).

/* $example_19_1$
--- ex19.pl (source)
+++ ex19.pl (target)
@@ -1,6 +1,6 @@
 :- module(ex19, [ex19/2, ex19/3]).
 
-ex19(_C, (2,3)).
+ex19((2,3), (2,3)).
 
 ex19(f(/*1*/A, 0 ), _B, f(/*2*/b, f(/*3*/A, 0 ))).
 
*/

test(example_19_1) :-
    [ex19],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex19:ex19(C,D), ex19(C,D), C=D, [])),
    comment_data(example_19_1, Pattern),
    assertion(Pattern == Result).

/* $example_19_2$
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

test(example_19_2) :-
    [ex19],
    rreset,
    with_output_to(string(Result),
		   expand_sentence(ex19:ex19(A,B,C), ex19(A, B), B=C, [])),
    comment_data(example_19_2, Pattern),
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
		   replace_conjunction(conjex:_, (a(A),b(B)), c(A-B), [])),
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
    with_output_to(string(Result1), replace_term(conjex:_,a(B),aa(B),[])),
    comment_data(two_changes_1, Pattern1),
    assertion(Pattern1 == Result1),
    with_output_to(string(Result2),
		   replace_term(conjex:_,aa(a),aa(b), [append])),
    comment_data(two_changes_2, Pattern2),
    assertion(Pattern2 == Result2),
    with_output_to(string(Result12), rshow),
    comment_data(two_changes_12, Pattern12),
    assertion(Pattern12 == Result12).

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
		   expand_sentence(ex21:ex21(X),ex21(Y),
				   (X=f(_A,B,C),Y=g(a,B,C)),[])),
    comment_data(ex21, Pattern),
    assertion(Pattern == Result).

:- comment_data:disable.

:- end_tests(refactor).
