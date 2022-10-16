:- begin_tests(seekn_parenthesis_right).

:- include(refactor_common).
:- use_module(seekn_parenthesis_right).

/* $seekn_parenthesis_right$
diff -ruN seekn_parenthesis_right.pl -
--- seekn_parenthesis_right.pl (source)
+++ seekn_parenthesis_right.pl (target)
@@ -1,5 +1,5 @@
 :- module(seekn_parenthesis_right, [a/2]).
 
-a({/**/{t}}, [f(_T), f(_P), f(_A)]).
+a({/**/{t}}, [f_(_T), f_(_P), f_(_A)]).
 
-a((/**/(t)), [f(_T), f(_P), f(_A)]).
+a((/**/(t)), [f_(_T), f_(_P), f_(_A)]).
*/

test(seekn_parenthesis_right) :-
    execute_test(seekn_parenthesis_right,
                 replace_term(f(D),f_(D)),
                 [file(seekn_parenthesis_right)]).

:- end_tests(seekn_parenthesis_right).
