:- use_module(library(plunit)).

:- begin_tests(refactor).

:- use_module(library(refactor)).
:- use_module(library(call_in_dir)).
:- use_module(library(comment_data)).
:- use_module(library(substitute)).
:- use_module(library(clambda)).

:- comment_data:enable.
:- set_setting(listing:tab_distance, 0). % Use only spaces, no tabs

:- use_module(ex1).

:- meta_predicate
    execute_test(+,1),
    execute_test(+,1,+),
    execute_test(+,+,1,+).

execute_test(Module, Goal) :-
    execute_test(Module, Module, Goal, []).

execute_test(Module, Test, Goal, OptionL) :-
    execute_test(Test, Goal, [module(Module)|OptionL]).

execute_test(Test, Goal, OptionL) :-
    rreset,
    call_in_module_dir(ex1,
                       ( call(Goal, OptionL),
                         with_output_to(string(Result), rshow)
                       )),
    comment_data(Test, Pattern),
    ( Pattern \== Result
    ->format("~s", [Result])
    ; true
    ),
    assertion(Pattern == Result).

:- redefine_system_predicate(current_op(_,_,_)).
current_op(A,B,C) :-
    catch(current_op(A,B,C),_E, backtrace(20)).

/* $ex1$
diff -ruN ex1.pl -
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
diff -ruN ex2.pl -
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
diff -ruN ex3.pl -
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
diff -ruN ex4.pl -
--- ex4.pl (source)
+++ ex4.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex4, [ex4/2]).
 
-ex4(_A, b).
+ex4_(f(a), b).
*/

test(ex4) :-
    execute_test(ex4, replace_sentence(ex4(_A, B), ex4_(X, B), (X=f(a)))).

:- use_module(ex5).

/* $ex5$
diff -ruN ex5.pl -
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
diff -ruN ex6.pl -
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
diff -ruN ex7.pl -
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
diff -ruN ex7.pl -
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
diff -ruN ex8.pl -
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
diff -ruN ex9.pl -
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
diff -ruN ex10.pl -
--- ex10.pl (source)
+++ ex10.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex10, [ex10/2]).
 
-ex10(f(A), g(A)).
+ex10(f(A), g(C, a(C))).
*/

test(ex10_1) :-
    execute_test(ex10, ex10_1,
                 replace_term(g(_A), g(B,X), ((X=a(B),B='$VAR'('C')))),
                 [sentence(ex10(_, _))]).

/* $ex10_2$
diff -ruN ex10.pl -
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
diff -ruN ex11.pl -
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
diff -ruN ex12.pl -
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
diff -ruN ex13.pl -
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
    once(clause(ex13:ex13(_,_), _, Ref)),
    execute_test(ex13, replace_term(T, T1$@T,
                                    ( nonvar(T),
                                      T=q(_B,A),
                                      var(A),
                                      substitute_value(A, a, T, T1)
                                    )), [clause(Ref)]).

:- use_module(ex14).

/* $ex14_1$
diff -ruN ex14.pl -
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
+ex14((B, B), _C) :-
     true.
 
-ex14(A, B) :-
-    A = f([/**/B, _C]),
+ex14(f([/**/B, _C]), B) :-
     true.
 
-ex14(A, B) :-
-    f(A, 'b') = f(a, B),
-    \+ A,
-    \+ B.
+ex14(a, b) :-
+    \+ a,
+    \+ b.
 
-ex14(A, B) :-
-    B = [x|T],
+ex14(A, [x|T]) :-
     ex14(A, T).
 
 a.
*/

test(ex14_1) :-
    execute_test(ex14, ex14_1,
                 replace_sentence((Head :- A=B, Body), (Head1$@Head :- Body1$@Body), (unifiable(A,B,L),substitute_values(L,Head-Body,Head1-Body1))), []).

/* $ex14_2$
diff -ruN ex14.pl -
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
diff -ruN ex15.pl -
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
    execute_test(ex15, replace_sentence(ex15(L,A), [ex15(L1$@L)], (substitute_value(A, a, L, L1)))).

:- use_module(ex16).

/* $ex16$
*/

test(ex16) :-
    execute_test(ex16, replace_sentence(H, H, true)).

:- use_module(ex17).

/* $ex17$
diff -ruN ex17.pl -
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
diff -ruN ex18.pl -
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
    execute_test(ex18, replace_sentence((H:-A=B,p(C)), (H1:-p(C1)), substitute_value(A,B,H-C,H1-C1))).

:- use_module(ex19).

/* $ex19_1$
diff -ruN ex19.pl -
--- ex19.pl (source)
+++ ex19.pl (target)
@@ -1,6 +1,6 @@
 :- module(ex19, [ex19/2, ex19/3]).
 
-ex19(_C, (2,3)).
+ex19((2,3), (2,3)).
 
 ex19(f(/*1*/A, 0 ), _B, f(/*2*/b, f(/*3*/A, 0 ))).
 
*/

test(ex19_1) :-
    execute_test(ex19, ex19_1, replace_sentence(ex19(_C,D), ex19(D,D)), []).

/* $ex19_2$
diff -ruN ex19.pl -
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
    execute_test(ex19, ex19_2,
                 replace_sentence(ex19(A, B, C), ex19(AS$@A, C),
                                  substitute_value(B, C, A, AS)), []).

:- use_module(conjex).

/* $conjex$
diff -ruN conjex.pl -
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
diff -ruN conjex.pl -
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
diff -ruN conjex.pl -
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
diff -ruN conjex.pl -
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
    call_in_module_dir(conjex, test_two_changes).

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
diff -ruN ex21.pl -
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
diff -ruN ex22.pl -
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
diff -ruN ex23.pl -
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
diff -ruN ex24.pl -
--- ex24.pl (source)
+++ ex24.pl (target)
@@ -1,4 +1,4 @@
 :- module(ex24, [ex24/1]).
 
 ex24(A) :-
-    A = /****/ key_components/4+ (/*1*/ ( hidden ), ( kbmask([+, +, -, -]) ) /*2*/).
+    A = /****/ key_components/4+ (/*1*/ ( help ), (/*1*/ ( hidden ), ( kbmask([+, +, -, -]) ) /*2*/) /*2*/).
*/

test(ex24) :-
    execute_test(ex24, replace_term(A/B+P, A/B+(help,P))).

:- use_module(ex26).

/* $ex26$
diff -ruN ex26.pl -
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
diff -ruN ex27.pl -
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
    execute_test(ex27, replace_term((ex27:- (A=V,Body)), (ex27 :- (Body1@@Body)@@(A=V,Body)),
                                    substitute_value(A, V, Body, Body1))).

/* $ex27_2$
diff -ruN ex27.pl -
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

:- use_module(exapp).

/* $exapp_1$
diff -ruN exapp.pl -
--- exapp.pl (source)
+++ exapp.pl (target)
@@ -1,15 +1,13 @@
 :- module(exapp, [exapp/3]).
 :- style_check(-singleton).
 exls(L) :-
-    append([a], /* 0 */ [ /* 1 */ ] /* 2 */, L).
+    L = [a/* 0 */  /* 1 */  /* 2 */].
 exls(L) :-
-    append([a], [f(_B) /* 1 */] /*2*/, L).
+    L = [a, f(_B) /* 1 */ /*2*/].
 exls(L) :-
-    append([a], [f(b)], L).
+    L = [a, f(b)].
 
 exapp(A, T, C) :-
-    append([ /*1*/A,
-             /*2*/A], /*3*/ T, C).
+    C = [/*1*/A, /*2*/A|/*3*/ T].
 exapp(A1-A2, T, C) :-
-    append([ [ _, [ A1 ] ] ], [ [ _, [ A2 ] ], [ _, [ T ] ] ],
-           C).
+    C = [[ _, [ A1 ] ],  [ _, [ A2 ] ], [ _, [ T ] ] ].
*/

test(exapp_1) :-
    execute_test(exapp, exapp_1,
                 replace_term(append(A,B,C), C=L, (is_list(A),append(A,B,L))),
                 [linear_term(yes)]).

/* $exapp_2$
diff -ruN exapp.pl -
--- exapp.pl (source)
+++ exapp.pl (target)
@@ -1,15 +1,14 @@
 :- module(exapp, [exapp/3]).
 :- style_check(-singleton).
 exls(L) :-
-    append([a], /* 0 */ [ /* 1 */ ] /* 2 */, L).
+    L = [a/* 0 */  /* 2 */].
 exls(L) :-
-    append([a], [f(_B) /* 1 */] /*2*/, L).
+    L = [a, f(_B) /* 1 */ /*2*/].
 exls(L) :-
-    append([a], [f(b)], L).
+    L = [a, f(b)].
 
 exapp(A, T, C) :-
-    append([ /*1*/A,
-             /*2*/A], /*3*/ T, C).
+    C = [ /*1*/A,
+             /*2*/A|/*3*/ T].
 exapp(A1-A2, T, C) :-
-    append([ [ _, [ A1 ] ] ], [ [ _, [ A2 ] ], [ _, [ T ] ] ],
-           C).
+    C = [ [ _, [ A1 ] ], [ _, [ A2 ] ], [ _, [ T ] ] ].
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
    tmp_file_stream(text, File, Stream),
    close(Stream),
    copy_file(Ex1, File),
    [File],
    rreset,
    replace_term((same_term(c,a),d,b),((d,b)), [module(ex1_)]),
    with_output_to(string(Result), rshow),
    assertion(Result\==""),
    rcommit.

:- use_module(exge).

/* $exge$
diff -ruN exge.pl -
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
diff -ruN exge.pl -
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
diff -ruN exnoload.pl -
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
    execute_test(exnoload, replace_goal(exnoload(A,B), 'exnoload*'(A,B)),
                 [files(exnoload)]).

/* $opex1_1$
diff -ruN opex1.pl -
--- opex1.pl (source)
+++ opex1.pl (target)
@@ -7,5 +7,5 @@
 A myis B :- display(A myis B), nl.
 
 opex1(A, B) :-
-    A myis B.
+    p(A, B).
 
*/

test(opex1_1) :-
    execute_test(opex1_1, replace_goal(myis(A, B), p(A, B)), [files(opex1)]).

/* $opex1_2$
diff -ruN opex1.pl -
--- opex1.pl (source)
+++ opex1.pl (target)
@@ -7,5 +7,5 @@
 A myis B :- display(A myis B), nl.
 
 opex1(A, B) :-
-    A myis B.
+    A myis2 B.
 
*/

test(opex1_2) :-
    execute_test(opex1_2, replace_goal(myis(A, B), myis2(A, B)), [files(opex1)]).

:- use_module(opex2).

/* $opex2$
diff -ruN opex2.pl -
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
                 replace(goal, myis(A, B), myis2(A, B), true), []).

:- use_module(fpex).

/* $fpex$
diff -ruN fpex.pl -
--- fpex.pl (source)
+++ fpex.pl (target)
@@ -1,9 +1,9 @@
 :- module(fpex, [fpex/2]).
 
 'fpex'(A, B) :-
-    once(( A=1,
+    once(( A=1->
            B=a
-         ; A=2,
+         ; A=2->
            B=b
          ; A=3,
            B=c
*/
test(fpex) :-
    execute_test(fpex, fpex,
                 replace_term(((A,B);C), (A->B;C), true), []).

/* $eqname_1$
diff -ruN eqname.pl -
--- eqname.pl (source)
+++ eqname.pl (target)
@@ -1,4 +1,4 @@
 :- module(eqname, [eqname/2]).
 
 eqname(A, (B, c)) :-
-    A + B  : (A -> B).
+    (A + B)  ^ (A -> B).
*/

test(eqname_1) :-
    execute_test(eqname_1, replace_term(A:B,A^B), [files(eqname)]).

/* $eqname_2$
diff -ruN eqname.pl -
--- eqname.pl (source)
+++ eqname.pl (target)
@@ -1,4 +1,4 @@
 :- module(eqname, [eqname/2]).
 
 eqname(A, (B, c)) :-
-    A + B  : (A -> B).
+    A + B  *-> (A -> B).
*/

test(eqname_2) :-
    execute_test(eqname_2, replace_term((A:B),A*->B),[files(eqname)]).

/* $opfp$
diff -ruN opfp.pl -
--- opfp.pl (source)
+++ opfp.pl (target)
@@ -1,4 +1,4 @@
 :- module(opfp, [opfp/1]).
 
 opfp(X) :-
-    X is 1 +  2 +   3 +    4.
+    X is ((1 ^  2) ^   3) ^    4.
*/

test(opfp) :-
    execute_test(opfp, replace_term(A+B, A^B),[files(opfp)]).

:- use_module(exsl).

/* $exsl$
diff -ruN exsl.pl -
--- exsl.pl (source)
+++ exsl.pl (target)
@@ -7,14 +7,14 @@
 p(c).
 p(d).
 
-% Hello world
-% This comment belongs to f(a)
-
-f(a).
-
 % more
 
 % There we go
 f(b).
+% Hello world
+% This comment belongs to f(a)
+
+f(a).
+f(d).
 
 % don't delete this.
*/

test(exsl) :-
    execute_test(exsl, replace_sentence([f(a),f(b)],[f(b),f(a),f(d)])).

/* $exsl2$
diff -ruN exsl.pl -
--- exsl.pl (source)
+++ exsl.pl (target)
@@ -1,11 +1,6 @@
 :- module(exsl, [f/1]).
 
-% proper merging:
-
-p(a).
-p(b).
-p(c).
-p(d).
+p([a, b, c, d]).
 
 % Hello world
 % This comment belongs to f(a)
*/
test(exsl2) :-
    execute_test(exsl2, replace_sentence([p(A),p(B)],p(C),flatten([A,B],C)),[file(exsl), fixpoint(true)]).

:- use_module(exst).

/* $exst$
diff -ruN exst.pl -
--- exst.pl (source)
+++ exst.pl (target)
@@ -1,3 +1,3 @@
 :- module(exst, [p/1]).
 
-p([a,b,c,d]).
+p([a-"A",b-"B",c-"C",d-"D"]).
*/

test(exst) :-
    execute_test(exst, exst, replace_term(X, X-Y, (atom(X), string_upper(X, Y))), [sentence(p(_))]).

/* $ref_body$
diff -ruN ref_body.pl -
--- ref_body.pl (source)
+++ ref_body.pl (target)
@@ -3,7 +3,9 @@
 rb :-
     call_cleanup(call,
                  % 1
-                 cleanup).
+                 ( cleanup1(a),
+                   cleanup2(a)
+                 )).
 
 call.
 
@@ -12,7 +14,8 @@
 rb2 :-
     call,
     /* 1 */
-    cleanup,
+    cleanup1(a),
+    cleanup2(a),
     done.
 
 cleanup :-
*/

test(ref_body) :-
    execute_test(ref_body, replace_conjunction(cleanup, (cleanup1(a),cleanup2(a))), [file(ref_body)]).

/* $ref_body2$
diff -ruN ref_body.pl -
--- ref_body.pl (source)
+++ ref_body.pl (target)
@@ -22,4 +22,4 @@
 cleanup1(_).
 
 p :-
-    forall(q, (a, b, c)).
+    forall(q, (b, c)).
*/

test(ref_body2) :-
    rreset,
    execute_test(ref_body2, replace_conjunction((a,b), b), [file(ref_body)]).

/* $addlit$
diff -ruN addlit.pl -
--- addlit.pl (source)
+++ addlit.pl (target)
@@ -5,11 +5,13 @@
 p3(_).
 
 q1(A) :-
-    p1(A),
+    test1,
     p2,
+    p1(A),
     p3(A).
 
 q1(A) :-
     p3(A),
-    p1(A),
-    p2.
+    test1,
+    p2,
+    p1(A).
*/

test(addlit) :-
    execute_test(addlit, replace_conjunction((p1(A), p2), (test1, p2, p1(A))), [file(addlit)]).

/* $newvars$
diff -ruN newvars.pl -
--- newvars.pl (source)
+++ newvars.pl (target)
@@ -1,2 +1,2 @@
 
-p(a(_N), R) :- R = g(f(a), "b").
+p(N, A1, R) :- R = g(f(N), A1, A2, A2, _, "b").
*/

test(newvars) :-
    execute_test(newvars,
                  \ OptionL
                 ^( replace_sentence((H1 :- B1), (H2 :- B2),
                                     (H1 = p(a(X), B),
                                      H2 = p(X, Y, B),
                                      B1 = (R = g(_, "b")),
                                      B2 = (R = g(f(X), Y, A, A, _D, "b"))
                                     ), OptionL),
                    remove_underscore_multi(OptionL),
                    anonymize_all_singletons(OptionL)
                  ),
                  [file(newvars), vars_preffix('A')]).

/* $autovn$
diff -ruN autovn.pl -
--- autovn.pl (source)
+++ autovn.pl (target)
@@ -1,5 +1,5 @@
 :- module(autovn, [f/6]).
 
-f(X, X, _, _, _Y, _Z, V1/V1).
+f(_X, V2, V2, _, Y, Y, _Z, V1/V1).
 
 p(g( _), _Y).
*/

test(autovn) :-
    execute_test(autovn,
                  \ OptionL
                 ^( replace_sentence(f(A1, _A2, A3, A4, A5, A6, A7),
                                     f(A1, A3, A3, A4, A5, A5, A6, A7),
                                     OptionL),
                    remove_underscore_multi(OptionL),
                    underscore_singletons(OptionL)
                  ),
                  [file(autovn)]).

/* $autovn2$
diff -ruN autovn.pl -
--- autovn.pl (source)
+++ autovn.pl (target)
@@ -2,4 +2,4 @@
 
 f(X, X, _, _, _Y, _Z, V1/V1).
 
-p(g( _), _Y).
+p(g( _), f(_)).
*/

test(autovn2) :-
    execute_test(autovn2,
                  \ OptionL
                 ^( replace_sentence(p(C,X), p(C,X), (X=f(_A)), OptionL),
                    anonymize_singletons(OptionL)
                  ),
                  [file(autovn)]).

/* $autovn3$
diff -ruN autovn.pl -
--- autovn.pl (source)
+++ autovn.pl (target)
@@ -2,4 +2,4 @@
 
 f(X, X, _, _, _Y, _Z, V1/V1).
 
-p(g( _), _Y).
+p(g( V1), V1).
*/

test(autovn3) :-
    execute_test(autovn3, replace_sentence(p(g(A),_B), p(g(A),A)),
                  [file(autovn)]).

/* $autovn4$
diff -ruN autovn.pl -
--- autovn.pl (source)
+++ autovn.pl (target)
@@ -2,4 +2,4 @@
 
 f(X, X, _, _, _Y, _Z, V1/V1).
 
-p(g( _), _Y).
+p(g( V1), t(V1)).
*/

test(autovn4) :-
    execute_test(autovn4, replace_sentence(p(g(A),_B), p(g(A),t(A))),
                  [file(autovn)]).

/* $list1$
diff -ruN list1.pl -
--- list1.pl (source)
+++ list1.pl (target)
@@ -1,2 +1,2 @@
-p([[A, B]]) :-
-  A = [B].
+p([[[B], B]]) :-
+  true.
*/
test(list1) :-
    execute_test(list1,
                  replace_sentence((H1 :- B1), (H2 :- B2),
                                   ( B1 = (A=B),
                                     occurrences_of_var(A, (H1 :- B1), 2),
                                     substitute_values([(A=B)=true, A=B],
                                                       (H1:-B1),(H2:-B2)))),
                  [file(list1)]).

/* $bind1$
diff -ruN bind1.pl -
--- bind1.pl (source)
+++ bind1.pl (target)
@@ -1,4 +1,4 @@
 
-f([[a, B, ""], B]).
+[["a", B, ""], B].
 
-f([['1', ""], '']).
+[["1", ""], ""].
*/
test(bind1) :- % tests the need of collapse the bindings
    execute_test(bind1,
                 replace_sentence(f(Text),$@(Text4),
                                  ( substitute(\ X^XS
                                              ^( atomic(X),
                                                 X \= []
                                               ->atom_string(X, XS)
                                               ), Text, Text3),
                                    copy_term(Text3, Text4),
                                    Text4=Text3
                                  )),
                 [file(bind1)]).

/* $decr$
diff -ruN decr.pl -
--- decr.pl (source)
+++ decr.pl (target)
@@ -1,2 +1,2 @@
 
-p([p(a),f(p(b))]) :- p(X),X=1.
+q([q(a),f(q(b))]) :- p(X),X=1.
*/

test(decr) :-
    execute_test(decr, replace(head_rec, p(A), q(A)), [file(decr)]).

/* $unfold1$
diff -ruN unfold.pl -
--- unfold.pl (source)
+++ unfold.pl (target)
@@ -1,4 +1,8 @@
 
 f(A, B) :-
-    append([1,2,3,4], A, X),
+    X = [1|V1],
+    V1 = [2|V2],
+    V2 = [3|V3],
+    V3 = [4|V1],
+    append([], A, V1),
     X = B.
*/

test(unfold1) :-
    execute_test(unfold1, replace_conjunction(append([E|X], Y, Z), (Z=[E|T], append(X, Y, T))), [file(unfold)]).

/* $unfold2$
diff -ruN unfold.pl -
--- unfold.pl (source)
+++ unfold.pl (target)
@@ -1,4 +1,8 @@
 
 f(A, B) :-
-    append([1,2,3,4], A, X),
+    append([], A, V4),
+    V3 = [4|V4],
+    V2 = [3|V3],
+    V1 = [2|V2],
+    X = [1|V1],
     X = B.
*/

test(unfold2) :-
    % In this case the structure is crecient, but to work around that the
    % decrease_metric was redefined in replace_conjunction
    execute_test(unfold2, replace_conjunction(append([E|X], Y, Z), (append(X, Y, T), Z=[E|T])), [file(unfold)]).

/* $unfold3$
diff -ruN unfold.pl -
--- unfold.pl (source)
+++ unfold.pl (target)
@@ -1,4 +1,8 @@
 
 f(A, B) :-
-    append([1,2,3,4], A, X),
+    append([], A, V4),
+    V3 = [4|V4],
+    V2 = [3|V3],
+    V1 = [2|V2],
+    X = [1|V1],
     X = B.
*/


test(unfold3) :-
    % In this case the structure is crecient, although the fixpoint terminates,
    % so we force its execution:
    rreset,
    execute_test(unfold3, replace_conjunction(append([E|X], Y, Z), (append(X, Y, T), Z=[E|T])), [fixpoint(true), file(unfold)]).

/* $empty1$
diff -ruN empty.pl -
--- empty.pl (source)
+++ empty.pl (target)
@@ -0,0 +1 @@
+a.
*/

test(empty1) :-
    execute_test(empty1, replace_sentence([], [a], true), [file(empty)]).

/* $empty2$
diff -ruN empty.pl -
--- empty.pl (source)
+++ empty.pl (target)
@@ -0,0 +1 @@
+a.
*/

test(empty2) :-
    execute_test(empty1, replace_sentence(end_of_file, [a], true), [file(empty)]).

/* $addini$
diff -ruN ex1.pl -
--- ex1.pl (source)
+++ ex1.pl (target)
@@ -1,3 +1,4 @@
+a.
 :- module(ex1, [g/0]).
 
 g :- same_term(c,a),d,(b   )   .
*/

test(addini) :-
    execute_test(addini, replace_sentence([], [a], true), [file(ex1)]).

/* $addend$
diff -ruN ex1.pl -
--- ex1.pl (source)
+++ ex1.pl (target)
@@ -5,3 +5,4 @@
 b.
 
 d.
+a.
*/

test(addend) :-
    execute_test(addend, replace_sentence(end_of_file, [a], true), [file(ex1)]).

/* $maxchg$
diff -ruN maxchg.pl -
--- maxchg.pl (source)
+++ maxchg.pl (target)
@@ -8,7 +8,7 @@
 p(c).
 
 s(A, B) :-
-    r(A, B).
+    rr(A, B).
 
 r(A, B) :-
     q(A, B).
*/

test(maxchg) :-
    execute_test(maxchg, replace_term(r(A,B), rr(A,B)), [max_changes(1), file(maxchg)]).

/* $meta1$
diff -ruN meta1.pl -
--- meta1.pl (source)
+++ meta1.pl (target)
@@ -2,14 +2,15 @@
 
 p(L, L2, L3) :-
     findall(E,
-            distinct(E,
-               ( member(E, L),
-                 E = ~a,
-                \+((member(E, L2),
-                    member(E, L3)
-                  )),
-                (once(sub_string(E, _, _, _, a))
-                ;once(sub_string(E, _, _, _, b))
-                )
-              )), RL),
+            order_by(asc(E),
+               distinct(E,
+                        ( member(E, L),
+                          E = ~a,
+                          \+ ( member(E, L2),
+                               member(E, L3)
+                             ),
+                          ( once(sub_string(E, _, _, _, a))
+                          ; once(sub_string(E, _, _, _, b))
+                          )
+                        ))), RL),
     writeln(user_error, RL).
*/

test(meta1) :-
    execute_test(meta1,
                 replace(body_rec,
                         findall(E, G, L),
                         findall(E, order_by(asc(E),
                                             '$BODYB'(G2)), L),
                         ( duplicate_term(G, G2),
                           G2=G
                         )), [file(meta1)]).

/* $singl$
diff -ruN singl.pl -
--- singl.pl (source)
+++ singl.pl (target)
@@ -1,2 +1,2 @@
 
-f(_, B).
+f(_, B, _).
*/

test(singl) :-
    execute_test(singl, replace_sentence(f(A,B),f(A,B,_)), [file(singl)]).

% not aded since it is too noisy:
% test(indent) :-
%     execute_test(indent, replace_sentence((A:-B), '$CLAUSE'(C),
%                                           ( duplicate_term((A:-B),C),
%                                             C=(A:-B)
%                                           )),
%                  [file('refactor.plt')]).

/* $atomic$
diff -ruN atomic.pl -
--- atomic.pl (source)
+++ atomic.pl (target)
@@ -1,4 +1,3 @@
 
 a :-
-    t(T),
-    l([T,e(n)]).
+    l([[t/n],e(n)]).
*/

test(atomic) :-
    execute_test(atomic,
                 replace_term((t(T), B), C$@B,
                              substitute_value(T, [t/n], B, C)), [file(atomic)]).

:- comment_data:disable.

:- end_tests(refactor).
