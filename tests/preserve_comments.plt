:- begin_tests(preserve_comments).

:- include(refactor_common).

:- use_module(preserve_comments).

/* $preserve_comments$
diff -ruN preserve_comments.pl -
--- preserve_comments.pl (source)
+++ preserve_comments.pl (target)
@@ -6,8 +6,8 @@
 
 preserve_comments :-
     % test1
-    X = (5,2), % test2
-    b(X).
+    % test2
+    b((5,2)).
 
 preserve_comments_2 :-
     @@(_A, _B).
*/

test(preserve_comments) :-
    execute_test(preserve_comments, replace_term((preserve_comments:- (A=V,Body)), (preserve_comments :- (Body1@@Body)@@(A=V,Body)),
                                    substitute_value(A, V, Body, Body1))).

/* $preserve_comments_2$
diff -ruN preserve_comments.pl -
--- preserve_comments.pl (source)
+++ preserve_comments.pl (target)
@@ -9,7 +9,6 @@
     X = (5,2), % test2
     b(X).
 
-preserve_comments_2 :-
-    @@(_A, _B).
+_B@@_A.
 
 @@(_, _).
*/

test(preserve_comments_2) :-
    execute_test(preserve_comments, preserve_comments_2, replace_sentence((preserve_comments_2:- @@(A,B)), (\\(@@(B,A)))), []).

:- end_tests(preserve_comments).
