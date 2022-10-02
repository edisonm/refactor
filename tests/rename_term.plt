:- begin_tests(rename_term).

:- include(refactor_common).

:- use_module(rename_term).

/* $rename_term$
diff -ruN rename_term.pl -
--- rename_term.pl (source)
+++ rename_term.pl (target)
@@ -1,3 +1,3 @@
 :- module(rename_term, [rename_term/1]).
 
-rename_term(f(b,c,   _D)).
+rename_term(g(a,c,   _D)).
*/

test(rename_term) :-
    execute_test(rename_term, replace_sentence(rename_term(X),rename_term(Y), ((X=f(_A,B,C),Y=g(a,B,C))))).

:- end_tests(rename_term).
