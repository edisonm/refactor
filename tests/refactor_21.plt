:- begin_tests(refactor_21).

:- include(refactor_common).

:- use_module(ex21).

/* $ex21$
diff -ruN ex21.pl -
--- ex21.pl (source)
+++ ex21.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex21, [ex21/1]).
 
-ex21(f(b,c,   _D)).
+ex21(g(a,c,   _D)).
*/

test(ex21) :-
    execute_test(ex21, replace_sentence(ex21(X),ex21(Y), ((X=f(_A,B,C),Y=g(a,B,C))))).

:- end_tests(refactor_21).
