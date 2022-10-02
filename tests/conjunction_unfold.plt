:- begin_tests(conjunction_unfold).

:- include(refactor_common).

/* $conjunction_unfold1$
diff -ruN conjunction_unfold.pl -
--- conjunction_unfold.pl (source)
+++ conjunction_unfold.pl (target)
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

test(conjunction_unfold1) :-
    execute_test(conjunction_unfold1, replace_conjunction(append([E|X], Y, Z), (Z=[E|T], append(X, Y, T))), [file(conjunction_unfold)]).

/* $conjunction_unfold2$
diff -ruN conjunction_unfold.pl -
--- conjunction_unfold.pl (source)
+++ conjunction_unfold.pl (target)
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

test(conjunction_unfold2) :-
    % In this case the structure is crecient, but to handle that problem,
    % decrease_metric was redefined in replace_conjunction
    execute_test(conjunction_unfold2, replace_conjunction(append([E|X], Y, Z), (append(X, Y, T), Z=[E|T])), [file(conjunction_unfold)]).

/* $conjunction_unfold3$
diff -ruN conjunction_unfold.pl -
--- conjunction_unfold.pl (source)
+++ conjunction_unfold.pl (target)
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


test(conjunction_unfold3) :-
    % In this case the structure is crecient, although the fixpoint terminates,
    % so we force its execution:
    rreset,
    execute_test(conjunction_unfold3, replace_conjunction(append([E|X], Y, Z), (append(X, Y, T), Z=[E|T])), [fixpoint(true), file(conjunction_unfold)]).

:- end_tests(conjunction_unfold).
