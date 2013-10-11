:- begin_tests(refactor).

:- use_module(library(refactor)).

:- use_module(ex1).
:- use_module(ex2).

example_1_out("\c
  --- ex1.pl (source)\c
\n+++ ex1.pl (target)\c
\n@@ -1,6 +1,6 @@\c
\n :- module(ex1, [g/0]).\c
\n \c
\n-g :- same_term(c,a),d,(b   )   .\c
\n+g :- d,(b   )   .\c
\n \c
\n list([[a,b],[c,d],[e]]).\c
\n \n").

example_2_out("\c
  --- ex1.pl (source)\c
\n+++ ex1.pl (target)\c
\n@@ -8,4 +8,4 @@\c
\n \c
\n aaa([[d, _]], [d], []).\c
\n \c
\n-f(a, f(/*1*/f( a)), f(/*2*/f( f(a  )))).\c
\n+g(f(/*2*/f( f(a  ))), f(/*1*/f( a)), a).\c
\n").

example_3_out("\c
  --- ex1.pl (source)\c
\n+++ ex1.pl (target)\c
\n@@ -2,9 +2,9 @@\c
\n \c
\n g :- same_term(c,a),d,(b   )   .\c
\n \c
\n-list([[a,b],[c,d],[e]]).\c
\n+list([[a,b], [e]]).\c
\n \c
\n-list([[a,b],[c,d]]).\c
\n+list([[a,b]]).\c
\n \c
\n aaa([[d, _]], [d], []).\c
\n \n").

example_4_out("\c
  --- ex1.pl (source)\c
\n+++ ex1.pl (target)\c
\n@@ -6,6 +6,6 @@\c
\n \c
\n list([[a,b],[c,d]]).\c
\n \c
\n-aaa([[d, _]], [d], []).\c
\n+aaa([[_]], [d], [[c|[d]]]).\c
\n \c
\n f(a, f(/*1*/f( a)), f(/*2*/f( f(a  )))).\c
\n").

test(example_1) :-
    with_output_to(codes(Result),
		   replace_term(ex1:_, (same_term(c,a),d,b),(d,b), show)),
    example_1_out(Pattern),
    assertion(Pattern == Result).

test(example_2) :-
    with_output_to(codes(Result),
		   expand_sentence(ex1:f(a,f(f(a)),C), g(C,f(f(a)),a),true,show)),
    example_2_out(Pattern),
    assertion(Pattern == Result).

test(example_3) :-
    gtrace,
    with_output_to(codes(Result),
		   expand_sentence(ex1:list([[a,b],[c,d]|T]), list([[a,b]|T]), true, show)),
    example_3_out(Pattern),
    assertion(Pattern == Result).

test(example_4) :-
    with_output_to(codes(Result),
		   expand_sentence(ex1:aaa([[d,_]],[d], []),
				   aaa([[_]], [d], [[c,d]]), true, show)),
    example_4_out(Pattern),
    assertion(Pattern == Result).

example_5_out("--- ex2.pl (source)\c
\n+++ ex2.pl (target)\c
\n@@ -1,7 +1,7 @@\c
\n :- module(ex2, []).\c
\n \c
\n q(A, B, L) :-\c
\n-    p(A, B, L, []).\c
\n+    p(B, A, L, []).\c
\n \c
\n p(_, _) --> [].\c
\n-p(A, B) --> p(A, B), \"hello\".\c
\n+p(A, B) --> p(B, A), \"hello\".\c
\n").

test(example_5) :-
    with_output_to(codes(Result),
		   replace_goal(ex2:_, ex2:p(A,B,L,T), p(B,A,L,T), show)),
    example_5_out(Pattern),
    assertion(Pattern == Result).

:- end_tests(refactor).
