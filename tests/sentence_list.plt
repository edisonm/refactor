:- begin_tests(sentence_list).

:- include(refactor_common).

/* $sentence_list_1$
diff -ruN sentence_list.pl -
--- sentence_list.pl (source)
+++ sentence_list.pl (target)
@@ -1 +1 @@
-a. /*b*/b. /*c*/c.
+/*b*/b. a. /*c*/c.
*/

test(sentence_list_1) :-
    execute_test(sentence_list_1, replace_sentence([a,b,X],[b,a,X]),[file(sentence_list)]).

/* $sentence_list_2$
diff -ruN sentence_list.pl -
--- sentence_list.pl (source)
+++ sentence_list.pl (target)
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

test(sentence_list_2) :-
    execute_test(sentence_list_2, replace_sentence([a,b,X],[b,a,a:-x,f(a),X]),[file(sentence_list)]).

:- end_tests(sentence_list).
