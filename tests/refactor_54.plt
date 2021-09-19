:- begin_tests(refactor_54).

:- include(refactor_common).

/* $ex54_1$
diff -ruN ex54.pl -
--- ex54.pl (source)
+++ ex54.pl (target)
@@ -1 +1 @@
-a. /*b*/b. /*c*/c.
+/*b*/b. a. /*c*/c.
*/

test(ex54_1) :-
    execute_test(ex54_1, replace_sentence([a,b,X],[b,a,X]),[file(ex54)]).

/* $ex54_2$
diff -ruN ex54.pl -
--- ex54.pl (source)
+++ ex54.pl (target)
@@ -1 +1,8 @@
-a. /*b*/b. /*c*/c.
+/*b*/b.
+
+a.
+a :- x.
+
+f(a).
+
+/*c*/c.
*/

test(ex54_2) :-
    execute_test(ex54_2, replace_sentence([a,b,X],[b,a,a:-x,f(a),X]),[file(ex54)]).

:- end_tests(refactor_54).
