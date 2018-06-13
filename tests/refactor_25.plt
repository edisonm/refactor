:- begin_tests(refactor_25).

:- include(refactor_common).

/* $singl$
diff -ruN singl.pl -
--- singl.pl (source)
+++ singl.pl (target)
@@ -1,2 +1,2 @@
 
-f(_, B).
+f(_, B, _).
*/

test(singl) :-
    execute_test(singl, replace_sentence(f(A,B),f(A,B,_)), [file(singl)]).

:- end_tests(refactor_25).
