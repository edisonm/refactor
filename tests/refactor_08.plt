:- begin_tests(refactor).

:- include(refactor_common).

:- use_module(ex8).

/* $ex8$
diff -ruN ex8.pl -
--- ex8.pl (source)
+++ ex8.pl (target)
@@ -1,5 +1,5 @@
 :- module(ex8, [ex8/1]).
 
-ex8([[a,b],[c,d],[e]]).
+ex8([[a,b], [e]]).
 
-ex8([[a,b],[c,d]]).
+ex8([[a,b]]).
*/

test(ex8) :-
    execute_test(ex8, replace_sentence(ex8([[a,b],[c,d]|T]), ex8([[a,b]|T]), true)).

:- end_tests(refactor).
