:- begin_tests(refactor).

:- include(refactor_common).

:- use_module(ex2).

/* $ex2$
diff -ruN ex2.pl -
--- ex2.pl (source)
+++ ex2.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex2, [f/3]).
 
-f(a, f(/*1*/f( a)), f(/*2*/f( f(a  )))).
+g(f(/*2*/f( f(a  ))), f(/*1*/f( a)), a).
*/

test(ex2) :-
    execute_test(ex2, replace_sentence(f(a,f(f(a)),C), g(C,f(f(a)),a),true)).

:- end_tests(refactor).
