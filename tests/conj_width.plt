:- begin_tests(conj_width).

:- include(refactor_common).

/* $conj_width1$
diff -ruN conj_width.pl -
--- conj_width.pl (source)
+++ conj_width.pl (target)
@@ -1,4 +1,16 @@
 
 p :-
-    l1,
+    ( a,
+      b,
+      c,
+      d,
+      e,
+      f,
+      g,
+      h,
+      i,
+      j,
+      k,
+      l
+    ),
     l2.
*/

test(conj_width1) :-
    execute_test(conj_width1,
                 replace_term(
                     l1,
                     (a,b,c,d,e,f,g,h,i,j,k,l)
                 ),
                [file(conj_width), conj_width(39)]).

/* $conj_width2$
diff -ruN conj_width.pl -
--- conj_width.pl (source)
+++ conj_width.pl (target)
@@ -1,4 +1,4 @@
 
 p :-
-    l1,
+    (a, b, c, d, e, f, g, h, i, j, k, l),
     l2.
*/

test(conj_width2) :-
    execute_test(conj_width2,
                 replace_term(
                     l1,
                     (a,b,c,d,e,f,g,h,i,j,k,l)
                 ),
                [file(conj_width), conj_width(40)]).

:- end_tests(conj_width).
