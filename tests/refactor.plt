:- begin_tests(refactor).

:- use_module(library(refactor)).

:- use_module(ex1).
:- use_module(ex2).
:- use_module(ex3).
:- use_module(ex4).
:- use_module(ex5).
:- use_module(ex6).
:- use_module(ex7).
:- use_module(ex8).
:- use_module(ex9).
:- use_module(ex10).
:- use_module(ex11).
:- use_module(ex12).
:- use_module(ex13).
:- use_module(ex14).

:- dynamic example_out/2.

%% process_refactor_test_data(+Comments, +Term) is semidet
% This comment_hook hack allow us to write copy-pasteable test data
% as a comment, to facilitate output comparisons:

process_refactor_test_data(Comments, Term) :-
    Term = (test(Test) :- _),
    format(string(Header), '/* $~w$~n', [Test]), % */
    member(_-Comment, Comments),
    string_concat(Header, Out0, Comment),
    string_concat(Out, '*/', Out0),
    retractall(example_out(Test, _)),
    assertz(example_out(Test, Out)).

:- multifile prolog:comment_hook/3.
:- dynamic prolog:comment_hook/3.

prolog:comment_hook(Comments, _TermPos, Term) :-
    process_refactor_test_data(Comments, Term).

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
    with_output_to(string(Result),
		   replace_term(ex1:_, (same_term(c,a),d,b),(d,b), show)),
    example_out(example_1, Pattern),
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
    with_output_to(string(Result),
		   expand_sentence(ex2:f(a,f(f(a)),C), g(C,f(f(a)),a),true,show)),
    example_out(example_2, Pattern),
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
    with_output_to(string(Result),
		   expand_sentence(ex3:(A :- display(B)), (A :- A, B, display(B)), true, show)),
    example_out(example_3, Pattern),
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
    with_output_to(string(Result),
		   expand_sentence(ex4:ex4(A, B), ex4_(A, B), (A=f(a)),show)),
    example_out(example_4, Pattern),
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
    with_output_to(string(Result),
		   expand_sentence(ex5:ex5(T),ex5([c|T]),true,show)),
    example_out(example_5, Pattern),
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
    with_output_to(string(Result),
		   replace_goal(ex6:_, ex6:p(A,B,L,T), p(B,A,L,T), show)),
    example_out(example_6, Pattern),
    assertion(Pattern == Result).

/* $example_7$
--- ex7.pl (source)
+++ ex7.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex7, [aaa/3]).
 
-aaa([[d, _]], [d /* d */], []).
+aaa([[_]], [d], [[c, d /* d */], [b, c, d /* d */]]).
*/

test(example_7) :-
    with_output_to(string(Result),
		   expand_sentence(ex7:aaa([[X,_]],[d], []),
				   aaa([[_]], [X], [[c,d],[b,c,d]]), true, show)),
    example_out(example_7, Pattern),
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
    with_output_to(string(Result),
		   expand_sentence(ex8:ex8([[a,b],[c,d]|T]), ex8([[a,b]|T]), true, show)),
    example_out(example_8, Pattern),
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
    with_output_to(string(Result),
		   expand_term(ex9:ex9(X, _), f(A,B), f(A,B,X), true, show)),
    example_out(example_9, Pattern),
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
    with_output_to(string(Result),
		   expand_term(ex10:ex10(_, _), g(A), g(B,A), (A=a(B),B='$VAR'('C')), show)),
    example_out(example_10_1, Pattern),
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
    with_output_to(string(Result),
		   expand_term(ex10:ex10(X, _), g(A), g(A,X), true, show)),
    example_out(example_10_2, Pattern),
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
    with_output_to(string(Result),
		   expand_term(ex11:(ex11([A|_]):-_), ex11(A), ex11_one(A), true, show)),
    example_out(example_11, Pattern),
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
    with_output_to(string(Result),
		   replace_term(ex12:_,(a,b),b,show)),
    example_out(example_12, Pattern),
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
    with_output_to(string(Result),
		   expand_term(ex13:_, T, T, (nonvar(T), T=q(_B,A),A=a), show)),
    example_out(example_13, Pattern),
    assertion(Pattern == Result).

/* $example_14_1$
--- ex14.pl (source)
+++ ex14.pl (target)
@@ -1,16 +1,13 @@
 :- module(ex14, [ex14/2]).
 
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
    with_output_to(string(Result),
		   expand_sentence(ex14:(Head :- A=B, Body), (Head :- Body), (A=B),show)),
    example_out(example_14_1, Pattern),
    assertion(Pattern == Result).

/* $example_14_2$
--- ex14.pl (source)
+++ ex14.pl (target)
@@ -1,7 +1,6 @@
 :- module(ex14, [ex14/2]).
 
-ex14(A, B) :-
-    A = f([/**/B, _C]),
+ex14(g(f([/**/B, _C])), B) :-
     true.
 
 ex14(A, B) :-
@@ -9,8 +8,7 @@
     \+ A,
     \+ B.
 
-ex14(A, B) :-
-    B = [x|T],
+ex14(A, g([x|T])) :-
     ex14(A, T).
 
 a.
*/

test(example_14_2) :-
    with_output_to(string(Result),
		   expand_sentence(ex14:(Head :- A=B, Body), (Head :- Body), (A=g(B)),show)),
    example_out(example_14_2, Pattern),
    assertion(Pattern == Result).

:- retractall(prolog:comment_hook(_, _, _)).

:- end_tests(refactor).
