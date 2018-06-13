:- begin_tests(refactor_24).

:- include(refactor_common).

:- use_module(ex24).

/* $ex24$
diff -ruN ex24.pl -
--- ex24.pl (source)
+++ ex24.pl (target)
@@ -1,4 +1,4 @@
 :- module(ex24, [ex24/1]).
 
 ex24(A) :-
-    A = /****/ key_components/4+ (/*1*/ ( hidden ), ( kbmask([+, +, -, -]) ) /*2*/).
+    A = /****/ key_components/4+ (/*1*/ ( help ), (/*1*/ ( hidden ), ( kbmask([+, +, -, -]) ) /*2*/) /*2*/).
*/

test(ex24) :-
    execute_test(ex24, replace_term(A/B+P, A/B+(help,P))).

:- end_tests(refactor_24).
