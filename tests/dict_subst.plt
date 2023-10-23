:- begin_tests(dict_subst).

:- include(refactor_common).

:- use_module(dict_subst).

% This two tests are together because in our first implementation they where
% incompatible

/* $dict1$
diff -ruN dict_subst.pl -
--- dict_subst.pl (source)
+++ dict_subst.pl (target)
@@ -2,5 +2,5 @@
 
 g(_X, k(_Z), a).
 
-f(x, _O{a:(b,   c)}).
+f(y, _O{a:(b,   c)}).
 
*/

test(dict1) :-
    execute_test(dict_subst, dict1, replace_term(f(x,A),f(y,A)), []).

/* $subst1$
diff -ruN dict_subst.pl -
--- dict_subst.pl (source)
+++ dict_subst.pl (target)
@@ -1,6 +1,6 @@
 :- module(dict_subst, [g/3, f/2]).
 
-g(_X, k(_Z), a).
+g(b, k(_Z), a).
 
 f(x, _O{a:(b,   c)}).
 
*/

test(subst1) :-
    execute_test(dict_subst, subst1, replace_term(g(X,Y,a), g(X,Y,a),(X=b)), []).

:- end_tests(dict_subst).
