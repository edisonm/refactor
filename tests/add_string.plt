:- begin_tests(add_string).

:- include(refactor_common).

:- use_module(add_string).

/* $add_string$
diff -ruN add_string.pl -
--- add_string.pl (source)
+++ add_string.pl (target)
@@ -1,3 +1,3 @@
 :- module(add_string, [p/1]).
 
-p([a,b,c,d]).
+p([a-"A",b-"B",c-"C",d-"D"]).
*/

test(add_string) :-
    execute_test(add_string, add_string, replace_term(X, X-Y, (atom(X), string_upper(X, Y))), [sentence(p(_))]).

:- end_tests(add_string).
