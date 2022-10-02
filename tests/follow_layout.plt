:- begin_tests(follow_layout).

:- include(refactor_common).
:- use_module(library(substitute)).

:- use_module(follow_layout).

/* $follow_layout_1$
diff -ruN follow_layout.pl -
--- follow_layout.pl (source)
+++ follow_layout.pl (target)
@@ -1,6 +1,6 @@
 :- module(follow_layout, [follow_layout/2, follow_layout/3]).
 
-follow_layout(_C, (2,3)).
+follow_layout((2,3), (2,3)).
 
 follow_layout(f(/*1*/A, 0 ), _B, f(/*2*/b, f(/*3*/A, 0 ))).
 
*/

test(follow_layout_1) :-
    execute_test(follow_layout, follow_layout_1, replace_sentence(follow_layout(_C,D), follow_layout(D,D)), []).

/* $follow_layout_2$
diff -ruN follow_layout.pl -
--- follow_layout.pl (source)
+++ follow_layout.pl (target)
@@ -2,8 +2,8 @@
 
 follow_layout(_C, (2,3)).
 
-follow_layout(f(/*1*/A, 0 ), _B, f(/*2*/b, f(/*3*/A, 0 ))).
+follow_layout(f(/*1*/A, 0 ), f(/*2*/b, f(/*3*/A, 0 ))).
 
-follow_layout([1|C], C, [2,3]).
+follow_layout([1, 2,3], [2,3]).
 
-follow_layout([/*1*/A], _B, [/**/b, A]).
+follow_layout([/*1*/A], [/**/b, A]).
*/

test(follow_layout_2) :-
    execute_test(follow_layout, follow_layout_2,
                 replace_sentence(follow_layout(A, B, C), follow_layout(AS$@A, C),
                                  substitute_value(B, C, A, AS)), []).

:- end_tests(follow_layout).
