:- begin_tests(keep_comments).

:- include(refactor_common).

:- use_module(keep_comments).

/* $keep_comments$
diff -ruN keep_comments.pl -
--- keep_comments.pl (source)
+++ keep_comments.pl (target)
@@ -1,3 +1,3 @@
 :- module(keep_comments, [f/3]).
 
-f(a, f(/*1*/f( a)), f(/*2*/f( f(a  )))).
+g(f(/*2*/f( f(a  ))), f(/*1*/f( a)), a).
*/

test(keep_comments) :-
    execute_test(keep_comments, replace_sentence(f(a,f(f(a)),C), g(C,f(f(a)),a),true)).

:- end_tests(keep_comments).
