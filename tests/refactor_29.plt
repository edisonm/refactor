:- begin_tests(refactor_29).

:- include(refactor_common).

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
                 [linearize([term])]).

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

%% This test will stop failing when we implement atomic replacements in with_context/8
test(exapp_2) :-
    execute_test(exapp, exapp_2,
                 replace_term(append(A,B,C),C=L$@A,(is_list(A),append(A,B,L))),
                 [linearize([atom, term])]).

/* $exapp_3$
diff -ruN exapp.pl -
--- exapp.pl (source)
+++ exapp.pl (target)
@@ -1,15 +1,13 @@
 :- module(exapp, [exapp/3]).
 :- style_check(-singleton).
 exls(L) :-
-    append([a], /* 0 */ [ /* 1 */ ] /* 2 */, L).
+    L = [a /* 1 */ ].
 exls(L) :-
-    append([a], [f(_B) /* 1 */] /*2*/, L).
+    L = [a, f(_B) /* 1 */].
 exls(L) :-
-    append([a], [f(b)], L).
+    L = [a, f(b)].
 
 exapp(A, T, C) :-
-    append([ /*1*/A,
-             /*2*/A], /*3*/ T, C).
+    C = [A, A|T].
 exapp(A1-A2, T, C) :-
-    append([ [ _, [ A1 ] ] ], [ [ _, [ A2 ] ], [ _, [ T ] ] ],
-           C).
+    C = [[ _, [ A1 ] ],  [ _, [ A2 ] ], [ _, [ T ] ] ].
*/

test(exapp_3) :-
    execute_test(exapp, exapp_3,
                 replace_term(append(A,B,C), C=L, (is_list(A),append(A,B,L))),
                 [subterm_boundary(subterm)]).

:- end_tests(refactor_29).
