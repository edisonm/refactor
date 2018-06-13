:- begin_tests(refactor_05).

:- include(refactor_common).

:- use_module(ex5).

/* $ex5$
diff -ruN ex5.pl -
--- ex5.pl (source)
+++ ex5.pl (target)
@@ -1,7 +1,7 @@
 :- module(ex5, [ex5/1]).
 
-ex5([]).
-ex5([/* hello */]).
-ex5([d]).
-ex5([d,e]).
-ex5(a).
+ex5([c]).
+ex5([c/* hello */]).
+ex5([c, d]).
+ex5([c, d,e]).
+ex5([c|a]).
*/

test(ex5) :-
    execute_test(ex5, replace_sentence(ex5(T), ex5([c|T]), true)).

:- end_tests(refactor_05).
