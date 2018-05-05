:- begin_tests(refactor).

:- include(refactor_common).

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

:- end_tests(refactor).
