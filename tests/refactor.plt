:- use_module(library(plunit)).

:- begin_tests(refactor).

:- use_module(library(refactor)).
:- use_module(library(call_in_module_file)).
:- use_module(library(comment_data)).

:- comment_data:enable.

:- use_module(ex1).

:- meta_predicate
    execute_test(+,1),
    execute_test_(+,1,+),
    execute_test(+,+,1,+).

execute_test(Module, Goal) :-
    execute_test(Module, Module, Goal, []).

execute_test(Module, Test, Goal, OptionL) :-
    execute_test_(Test, Goal, [module(Module)|OptionL]).

execute_test_(Test, Goal, OptionL) :-
    rreset,
    call_in_module_file(ex1,
			( call(Goal, OptionL),
			  with_output_to(string(Result), rshow)
			)),
    comment_data(Test, Pattern),
    assertion(Pattern == Result).

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
    execute_test(ex1, replace_term((((same_term(c,a),d,b))),(((d,b))))).

:- use_module(ex2).

/* $ex2$
--- ex2.pl (source)
+++ ex2.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex2, [f/3]).
 
-f(a, f(/*1*/f( a)), f(/*2*/f( f(a  )))).
+g(f(/*2*/f( f(a  ))), f(/*1*/f( a)), a).
*/

test(ex2) :-
    execute_test(ex2, replace_sentence(f(a,f(f(a)),C), g(C,f(f(a)),a),true)).

:- use_module(ex3).

/* $ex3$
--- ex3.pl (source)
+++ ex3.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex3, ['ex3'/0]).
 
-ex3 :- display('ex3').
+ex3 :- ex3, 'ex3', display('ex3').
*/

test(ex3) :-
    execute_test(ex3, replace_sentence((A :- display(B)),
				       (A :- A, B, display(B)), true)).

:- use_module(ex4).

/* $ex4$
--- ex4.pl (source)
+++ ex4.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex4, [ex4/2]).
 
-ex4(_A, b).
+ex4_(f(a), b).
*/

test(ex4) :-
    execute_test(ex4, replace_sentence(ex4(A, B), ex4_(A, B), (A=f(a)))).

:- use_module(ex5).

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
    execute_test(ex5, replace_sentence(ex5(T), ex5([c|T]), true)).

:- use_module(ex6).

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
    execute_test(ex6, replace_goal(p(A,B,L,T), p(B,A,L,T))).

:- use_module(ex7).

/* $ex7_1$
--- ex7.pl (source)
+++ ex7.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex7, [aaa/3]).
 
-aaa([[d, _]], [/**/ d /* d */], []).
+aab([[_]], e, [d], [[c, /**/ d /* d */], [b, c, /**/ d /* d */]]).
*/

test(ex7_1) :-
    execute_test(ex7, ex7_1,
		 replace_sentence(aaa([[X,_]],[d], []),
				  aab([['$VAR'('_')]], e, [X], [[c,d],[b,c,d]])), []).

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
    execute_test(ex7, ex7_2,
		 replace_sentence(aaa([[X,_]],[d], []),
				  aaa([['$VAR'('_')]], [X], [[c,d],[b,c,d]]), true), []).

:- use_module(ex8).

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
    execute_test(ex8, replace_sentence(ex8([[a,b],[c,d]|T]), ex8([[a,b]|T]), true)).

:- use_module(ex9).

/* $ex9$
--- ex9.pl (source)
+++ ex9.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex9, [ex9/2]).
 
-ex9(a, [f(g,c), g(d, e)]).
+ex9(a, [f(g, c, a), g(d, e)]).
*/

test(ex9) :-
    execute_test(ex9, ex9, replace_term(f(A,B), f(A,B,X)), [sentence(ex9(X, _))]).

:- use_module(ex10).

/* $ex10_1$
--- ex10.pl (source)
+++ ex10.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex10, [ex10/2]).
 
-ex10(f(A), g(A)).
+ex10(f(A), g(C, a(C))).
*/

test(ex10_1) :-
    execute_test(ex10, ex10_1,
		 replace_term(g(A), g(B,A), ((A=a(B),B='$VAR'('C')))),
		 [sentence(ex10(_, _))]).

/* $ex10_2$
--- ex10.pl (source)
+++ ex10.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex10, [ex10/2]).
 
-ex10(f(A), g(A)).
+ex10(f(A), g(A, f(A))).
*/

test(ex10_2) :-
    execute_test(ex10, ex10_2,
		 replace_term(g(A), g(A,X), true), [sentence(ex10(X, _))]).

:- use_module(ex11).

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
    execute_test(ex11, ex11,
		 replace_term(ex11(A), ex11_one(A), true),
		 [sentence((ex11([A|_]):-_))]).

:- use_module(ex12).

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
    execute_test(ex12, replace_term((a, b), b)).

:- use_module(ex13).

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
    execute_test(ex13, replace_term(T, T, (nonvar(T), T=q(_B,A),A=a))).

:- use_module(ex14).

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
    execute_test(ex14, ex14_1,
		 replace_sentence((Head :- A=B, Body), (Head :- Body), (A=B)), []).

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

    %% TODO: Fix this test!!!
testx(ex14_2) :-
    execute_test(ex14, ex14_2,
		 replace_sentence((Head :- A=B, Body), (Head :- Body), (A=g(B))), []).

:- use_module(ex15).

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
    execute_test(ex15, replace_sentence(ex15(L,A), [ex15(L)], (A=a))).

:- use_module(ex16).

/* $ex16$
*/

test(ex16) :-
    execute_test(ex16, replace_sentence(H, H, true)).

:- use_module(ex17).

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
    execute_test(ex17, replace_sentence((H:-(A,_B)), (H:-A))).

:- use_module(ex18).

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
    execute_test(ex18, replace_sentence((H:-A=B,p(C)), (H:-p(C)), A=B)).

:- use_module(ex19).

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
    execute_test(ex19, ex19_1, replace_sentence(ex19(C,D), ex19(C,D), C=D), []).

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
    execute_test(ex19, ex19_2, replace_sentence(ex19(A,B,C), ex19(A, B), B=C), []).

:- use_module(conjex).

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
    execute_test(conjex, replace_conjunction(((a(A),b(B))), c(A-B))).

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
    rreset,
    call_in_module_file(conjex, test_two_changes).

test_two_changes :-
    replace_term(a(B),aa(B),[module(conjex)]),
    with_output_to(string(Result1), rshow),
    with_output_to(string(ResultD), rdiff),
    assertion(a(ResultD) == a(Result1)),
    comment_data(two_changes_1, Pattern1),
    assertion(b(Pattern1) == b(Result1)),
    replace_term(aa(a),aa(b), [module(conjex)]),
    with_output_to(string(Result2), rdiff),
    comment_data(two_changes_2, Pattern2),
    assertion(c(Pattern2) == c(Result2)),
    with_output_to(string(Result12), rshow),
    comment_data(two_changes_12, Pattern12),
    assertion(d(Pattern12) == d(Result12)),
    once(rundo),
    with_output_to(string(Result3), rshow),
    assertion(e(Result3)==e(ResultD)),
    rsave('/tmp/two_changes.diff'),
    delete_file('/tmp/two_changes.diff').

:- use_module(ex21).

/* $ex21$
--- ex21.pl (source)
+++ ex21.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex21, [ex21/1]).
 
-ex21(f(b,c,   _D)).
+ex21(g(a,c,   _D)).
*/

test(ex21) :-
    execute_test(ex21, replace_sentence(ex21(X),ex21(Y), ((X=f(_A,B,C),Y=g(a,B,C))))).

:- use_module(ex22).

/* $ex22$
--- ex22.pl (source)
+++ ex22.pl (target)
@@ -1,4 +1,3 @@
 :- module(ex22, []).
 
-:- dynamic a/1  .
 
*/

test(ex22) :-
    execute_test(ex22, replace_sentence((:- dynamic _), [])).

:- use_module(ex23).

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
    execute_test(ex23, replace_term(A+B, A+(1*B))).

:- use_module(ex24).

/* $ex24$
--- ex24.pl (source)
+++ ex24.pl (target)
@@ -1,4 +1,4 @@
 :- module(ex24, [ex24/1]).
 
 ex24(A) :-
-    A = /****/ key_components/4+ (/*1*/ ( hidden ), ( kbmask([+, +, -, -]) ) /*2*/).
+    A = /****/ key_components/4+ (/*1*/ help, (/*1*/ ( hidden ), ( kbmask([+, +, -, -]) ) /*2*/) /*2*/).
*/

test(ex24) :-
    execute_test(ex24, replace_term(A/B+P, A/B+(help,P))).

:- use_module(ex26).

/* $ex26$
--- ex26.pl (source)
+++ ex26.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex26, [ex26/1]).
 
-ex26('$sb'(_, _, _, _, _, '$sb'(_, _, _, _, _, _))).
+ex26_('$sb'(_, _, _, _, _, '$sb'(_, _, _, _, _, _))).
*/

test(ex26) :-
    execute_test(ex26, replace_term(ex26(A),ex26_(A))).

:- use_module(ex27).

/* $ex27$
--- ex27.pl (source)
+++ ex27.pl (target)
@@ -6,8 +6,8 @@
 
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
    execute_test(ex27, replace_term((A=V,Body), A@@Body, A=V)).

/* $ex27_2$
--- ex27.pl (source)
+++ ex27.pl (target)
@@ -9,7 +9,6 @@
     X = (5,2), % test2
     b(X).
 
-ex27_2 :-
-    @@(_A, _B).
+_B@@_A.
 
 @@(_, _).
*/

test(ex27_2) :-
    execute_test(ex27, ex27_2, replace_sentence((ex27_2:- @@(A,B)), (\\(@@(B,A)))), []).

:- use_module(excomm).

/* $excomm_1$
*/

test(excomm_1) :-
    execute_test(excomm, excomm_1, replace_term(aaa, bbb), []).

/* $excomm_2$
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
--- excomm.pl (source)
+++ excomm.pl (target)
@@ -17,4 +17,4 @@
 
 f(a;(a,b,c)).
 
-f(a,(a,b,c)).
+f(a,(b,c)).
*/

test(excomm_3) :-
    execute_test(excomm, excomm_3, replace_sentence(f(a,(_,Body)), f(a,Body)), []).

:- use_module(exapp).

/* $exapp_1$
--- exapp.pl (source)
+++ exapp.pl (target)
@@ -1,8 +1,6 @@
 :- module(exapp, [exapp/3]).
 
 exapp(A, T, C) :-
-    append([ /*1*/A,
-	     /*2*/A], T, C).
+    C=[/*1*/A, /*2*/A|T].
 exapp(A1-A2, T, C) :-
-    append([ [ _, [ A1 ] ] ], [ [ _, [ A2 ] ], [ _, [ T ] ] ],
-	   C).
+    C=[[_, [A1]], [[_, [A2]], [_, [T]]]].
*/

test(exapp_1) :-
    execute_test(exapp, exapp_1,
		 replace_term(append(A,B,C), C=L, (is_list(A),append(A,B,L))),
		 [linear_term(yes)]).

/* $exapp_2$
--- exapp.pl (source)
+++ exapp.pl (target)
@@ -1,8 +1,7 @@
 :- module(exapp, [exapp/3]).
 
 exapp(A, T, C) :-
-    append([ /*1*/A,
-	     /*2*/A], T, C).
+    C=[ /*1*/A,
+	     /*2*/A| T].
 exapp(A1-A2, T, C) :-
-    append([ [ _, [ A1 ] ] ], [ [ _, [ A2 ] ], [ _, [ T ] ] ],
-	   C).
+    C=[ [ _, [ A1 ] ], [ _, [ A2 ] ], [ _, [ T ] ] ].
*/

test(exapp_2) :-
    execute_test(exapp, exapp_2,
		 replace_term(append(A,B,C),C=L$@A,(is_list(A),append(A,B,L))),
		 [linear_term(yes)]).

test(self_refactor_1) :-
    rreset,
    replace_term(print_expansion(A, B, C, D, E, F),
		 print_expansion_(A, B, C, D, E, F), [module(ref_replace)]),
    with_output_to(string(Result), rshow), assertion(Result \== "").

test(self_refactor_2) :-
    rreset,
    replace_term(rportray(A, B), rportray_(A, B), [module(ref_replace)]),
    with_output_to(string(Result), rshow), assertion(Result \== "").

test(save_changes) :-
    current_module(plunit_refactor, F),
    absolute_file_name('ex1_.pl', Ex1, [file_type(prolog), relative_to(F)]),
    copy_file(Ex1, '/tmp/ex1_.pl'),
    ['/tmp/ex1_.pl'],
    rreset,
    replace_term((((same_term(c,a),d,b))),(((d,b))), [module(ex1_)]),
    with_output_to(string(Result), rshow),
    assertion(Result\==""),
    rcommit.

:- use_module(exge).

/* $exge$
--- exge.pl (source)
+++ exge.pl (target)
@@ -7,12 +7,12 @@
     b~n".
 
 a(X) :-
-    exge:r,
-    call(r),
-    call(b(r), c, d(X)),
+    exge:r(1),
+    call(r(1)),
+    call(b(r(1)), c, d(X)),
     call(c, d).
 
-a --> b(r).
+a --> b(r(1)).
 
 d([a,b], a, b).
 
*/

test(exge) :-
    execute_test(exge, replace_goal(r,r(1))).

/* $exdcg$
--- exge.pl (source)
+++ exge.pl (target)
@@ -9,10 +9,10 @@
 a(X) :-
     exge:r,
     call(r),
-    call(b(r), c, d(X)),
+    call(b(r, s), c, d(X)),
     call(c, d).
 
-a --> b(r).
+a --> b(r, s).
 
 d([a,b], a, b).
 
*/
test(exdcg) :-
    execute_test(exge, exdcg, replace_goal(b(r,A,B),b(r,s,A,B)), []).

/* $exnoload$
--- exnoload.pl (source)
+++ exnoload.pl (target)
@@ -1,7 +1,7 @@
 :- module(exnoload, [exnoload/1]).
 
 exnoload(A) :-
-    exnoload(A, 2),
-    exnoload(A, 1).
+    'exnoload*'(A, 2),
+    'exnoload*'(A, 1).
 
 exnoload(A, A).
*/

test(exnoload) :-
    execute_test_(exnoload, replace_goal(exnoload(A,B), 'exnoload*'(A,B)),
		 [alias(exnoload)]).

/* $opex1_1$
--- opex1.pl (source)
+++ opex1.pl (target)
@@ -7,5 +7,5 @@
 A myis B :- display(A myis B), nl.
 
 opex1(A, B) :-
-    A myis B.
+    p(A, B).
 
*/

test(opex1_1) :-
    execute_test_(opex1_1, replace_goal(myis(A, B), p(A, B)), [alias(opex1)]).

/* $opex1_2$
--- opex1.pl (source)
+++ opex1.pl (target)
@@ -7,5 +7,5 @@
 A myis B :- display(A myis B), nl.
 
 opex1(A, B) :-
-    A myis B.
+    A myis2 B.
 
*/

test(opex1_2) :-
    execute_test_(opex1_2, replace_goal(myis(A, B), myis2(A, B)), [alias(opex1)]).

:- use_module(opex2).

/* $opex2$
--- opex2.pl (source)
+++ opex2.pl (target)
@@ -7,5 +7,5 @@
 A myis B :- display(A myis B), nl.
 
 opex2(A, B) :-
-    A myis B.
+    A myis2 B.
 
*/
test(opex2) :-
    execute_test(opex2, opex2,
		 replace(goal_cw,_:myis(A, B), myis2(A, B), true), []).

:- use_module(fpex).

/* $fpex$
--- fpex.pl (source)
+++ fpex.pl (target)
@@ -1,9 +1,9 @@
 :- module(fpex, [fpex/2]).
 
 'fpex'(A, B) :-
-    once(( A=1,
+    once(( A=1->
 	   B=a
-	 ; A=2,
+	 ; A=2->
 	   B=b
 	 ; A=3,
 	   B=c
*/
test(fpex) :-
    execute_test(fpex, fpex,
		 replace_term(((A,B);C), (A->B;C), true), [fixpoint(true)]).

/* $eqname_1$
--- eqname.pl (source)
+++ eqname.pl (target)
@@ -1,4 +1,4 @@
 :- module(eqname, [eqname/2]).
 
 eqname(A, (B, c)) :-
-    A + B  : (A -> B).
+    (A + B)  ^ (A -> B).
*/

test(eqname_1) :-
    execute_test(eqname, eqname_1, replace_term(A:B,A^B),[alias(eqname)]).

/* $eqname_2$
--- eqname.pl (source)
+++ eqname.pl (target)
@@ -1,4 +1,4 @@
 :- module(eqname, [eqname/2]).
 
 eqname(A, (B, c)) :-
-    A + B  : (A -> B).
+    A + B  *-> (A -> B).
*/

test(eqname_2) :-
    execute_test(eqname, eqname_2, replace_term(A:B,A*->B),[alias(eqname)]).

:- comment_data:disable.

:- end_tests(refactor).
