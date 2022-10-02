:- begin_tests(append_list).

:- include(refactor_common).

:- use_module(append_list).

/* $append_list_1$
diff -ruN append_list.pl -
--- append_list.pl (source)
+++ append_list.pl (target)
@@ -1,15 +1,14 @@
 :- module(append_list, [append_list/3]).
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
 
 append_list(A, T, C) :-
-    append([ /*1*/A,
-             /*2*/A], /*3*/ T, C).
+    C = [ /*1*/A,
+             /*2*/A|/*3*/ T].
 append_list(A1-A2, T, C) :-
-    append([ [ _, [ A1 ] ] ], [ [ _, [ A2 ] ], [ _, [ T ] ] ],
-           C).
+    C = [ [ _, [ A1 ] ] ,  [ _, [ A2 ] ], [ _, [ T ] ] ].
*/

test(append_list_1) :-
    execute_test(append_list, append_list_1,
                 replace_term(append(A,B,C), C='$APP'(A, B), is_list(A)),
                 [fixpoint(none),linearize([vars])]).

/* $append_list_3$
diff -ruN append_list.pl -
--- append_list.pl (source)
+++ append_list.pl (target)
@@ -1,15 +1,14 @@
 :- module(append_list, [append_list/3]).
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
 
 append_list(A, T, C) :-
-    append([ /*1*/A,
-             /*2*/A], /*3*/ T, C).
+    C = [ /*1*/A,
+             /*2*/A|T].
 append_list(A1-A2, T, C) :-
-    append([ [ _, [ A1 ] ] ], [ [ _, [ A2 ] ], [ _, [ T ] ] ],
-           C).
+    C = [ [ _, [ A1 ] ] ,  [ _, [ A2 ] ], [ _, [ T ] ] ].
*/

test(append_list_3) :-
    execute_test(append_list, append_list_3,
                 replace_term(append(A,B,C), C='$APP'(A,B), is_list(A)),
                 [subterm_boundary(subterm)]).

:- end_tests(append_list).
