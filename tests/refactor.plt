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

example_1_out("\c
  --- ex1.pl (source)\c
\n+++ ex1.pl (target)\c
\n@@ -1,3 +1,3 @@\c
\n :- module(ex1, [g/0]).\c
\n \c
\n-g :- same_term(c,a),d,(b   )   .\c
\n+g :- d,(b   )   .\c
\n").

test(example_1) :-
    with_output_to(codes(Result),
		   replace_term(ex1:_, (same_term(c,a),d,b),(d,b), show)),
    example_1_out(Pattern),
    assertion(Pattern == Result).

example_2_out("\c
  --- ex2.pl (source)\c
\n+++ ex2.pl (target)\c
\n@@ -1,3 +1,3 @@\c
\n :- module(ex2, [f/3]).\c
\n \c
\n-f(a, f(/*1*/f( a)), f(/*2*/f( f(a  )))).\c
\n+g(f(/*2*/f( f(a  ))), f(/*1*/f( a)), a).\c
\n").

test(example_2) :-
    with_output_to(codes(Result),
		   expand_sentence(ex2:f(a,f(f(a)),C), g(C,f(f(a)),a),true,show)),
    example_2_out(Pattern),
    assertion(Pattern == Result).

example_3_out("--- ex3.pl (source)\c
\n+++ ex3.pl (target)\c
\n@@ -1,3 +1,3 @@\c
\n :- module(ex3, ['ex3'/0]).\c
\n \c
\n-ex3 :- display('ex3').\c
\n+ex3 :- ex3, 'ex3', display('ex3').\c
\n").

test(example_3) :-
    with_output_to(codes(Result),
		   expand_sentence(ex3:(A :- display(B)), (A :- A, B, display(B)), true, show)),
    example_3_out(Pattern),
    assertion(Pattern == Result).

example_4_out("--- ex4.pl (source)\c
\n+++ ex4.pl (target)\c
\n@@ -1,3 +1,3 @@\c
\n :- module(ex4, [ex4/2]).\c
\n \c
\n-ex4(_A, b).\c
\n+ex4_(f(a), b).\c
\n").

test(example_4) :-
    with_output_to(codes(Result),
		   expand_sentence(ex4:ex4(A, B), ex4_(A, B), (A=f(a)),show)),
    example_4_out(Pattern),
    assertion(Pattern == Result).    

example_5_out("--- ex5.pl (source)\c
\n+++ ex5.pl (target)\c
\n@@ -1,7 +1,7 @@\c
\n :- module(ex5, [ex5/1]).\c
\n \c
\n-ex5([]).\c
\n-ex5([/* hello */]).\c
\n-ex5([d]).\c
\n-ex5([d,e]).\c
\n-ex5(a).\c
\n+ex5([c]).\c
\n+ex5([c/* hello */]).\c
\n+ex5([c, d]).\c
\n+ex5([c, d,e]).\c
\n+ex5([c|a]).\c
\n").

test(example_5) :-
    with_output_to(codes(Result),
		   expand_sentence(ex5:ex5(T),ex5([c|T]),true,show)),
    example_5_out(Pattern),
    assertion(Pattern == Result).

example_6_out("--- ex6.pl (source)\c
\n+++ ex6.pl (target)\c
\n@@ -1,7 +1,7 @@\c
\n :- module(ex6, []).\c
\n \c
\n q(A, B, L) :-\c
\n-    p(A, B, L, []).\c
\n+    p(B, A, L, []).\c
\n \c
\n p(_, _) --> [].\c
\n-p(A, B) --> p(A, B), \"hello\".\c
\n+p(A, B) --> p(B, A), \"hello\".\c
\n").

test(example_6) :-
    with_output_to(codes(Result),
		   replace_goal(ex6:_, ex6:p(A,B,L,T), p(B,A,L,T), show)),
    example_6_out(Pattern),
    assertion(Pattern == Result).

example_7_out("\c
  --- ex7.pl (source)\c
\n+++ ex7.pl (target)\c
\n@@ -1,3 +1,3 @@\c
\n :- module(ex7, [aaa/3]).\c
\n \c
\n-aaa([[d, _]], [d /* d */], []).\c
\n+aaa([[_]], [d], [[c, d /* d */], [b, c, d /* d */]]).\c
\n").

test(example_7) :-
    with_output_to(codes(Result),
		   expand_sentence(ex7:aaa([[X,_]],[d], []),
				   aaa([[_]], [X], [[c,d],[b,c,d]]), true, show)),
    example_7_out(Pattern),
    assertion(Pattern == Result).

example_8_out("\c
  --- ex8.pl (source)\c
\n+++ ex8.pl (target)\c
\n@@ -1,5 +1,5 @@\c
\n :- module(ex8, [ex8/1]).\c
\n \c
\n-ex8([[a,b],[c,d],[e]]).\c
\n+ex8([[a,b], [e]]).\c
\n \c
\n-ex8([[a,b],[c,d]]).\c
\n+ex8([[a,b]]).\c
\n").

test(example_8) :-
    with_output_to(codes(Result),
		   expand_sentence(ex8:ex8([[a,b],[c,d]|T]), ex8([[a,b]|T]), true, show)),
    example_8_out(Pattern),
    assertion(Pattern == Result).

example_9_out("\c
  --- ex9.pl (source)\c
\n+++ ex9.pl (target)\c
\n@@ -1,3 +1,3 @@\c
\n :- module(ex9, [ex9/2]).\c
\n \c
\n-ex9(a, [f(g,c), g(d, e)]).\c
\n+ex9(a, [f(g, c, a), g(d, e)]).\c
\n").

test(example_9) :-
    with_output_to(codes(Result),
		   expand_term(ex9:ex9(X, _), f(A,B), f(A,B,X), true, show)),
    example_9_out(Pattern),
    assertion(Pattern == Result).

example_10_1_out("\c
  --- ex10.pl (source)\c
\n+++ ex10.pl (target)\c
\n@@ -1,3 +1,3 @@\c
\n :- module(ex10, [ex10/2]).\c
\n \c
\n-ex10(f(A), g(A)).\c
\n+ex10(f(A), g(C, a(C))).\c
\n").

test(example_10_1) :-
    with_output_to(codes(Result),
		   expand_term(ex10:ex10(_, _), g(A), g(B,A), (A=a(B),B='$VAR'('C')), show)),
    example_10_1_out(Pattern),
    assertion(Pattern == Result).

example_10_2_out("\c
  --- ex10.pl (source)\c
\n+++ ex10.pl (target)\c
\n@@ -1,3 +1,3 @@\c
\n :- module(ex10, [ex10/2]).\c
\n \c
\n-ex10(f(A), g(A)).\c
\n+ex10(f(A), g(A, f(A))).\c
\n").

test(example_10_2) :-
    with_output_to(codes(Result),
		   expand_term(ex10:ex10(X, _), g(A), g(A,X), true, show)),
    example_10_2_out(Pattern),
    assertion(Pattern == Result).

example_11_out("\c
  --- ex11.pl (source)\c
\n+++ ex11.pl (target)\c
\n@@ -1,5 +1,5 @@\c
\n :- module(ex11, [ex11/1]).\c
\n \c
\n ex11([A|B]) :-\c
\n-    ex11(A),\c
\n+    ex11_one(A),\c
\n     ex11(B).\c
\n").

test(example_11) :-
    with_output_to(codes(Result),
		   expand_term(ex11:(ex11([A|_]):-_), ex11(A), ex11_one(A), true, show)),
    example_11_out(Pattern),
    assertion(Pattern == Result).

:- end_tests(refactor).
