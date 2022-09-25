:- begin_tests(add_arg_sent).

:- include(refactor_common).

:- use_module(add_arg_sent).

/* $add_arg_sent$
diff -ruN add_arg_sent.pl -
--- add_arg_sent.pl (source)
+++ add_arg_sent.pl (target)
@@ -1,3 +1,3 @@
 :- module(add_arg_sent, [add_arg_sent/2]).
 
-add_arg_sent(a, [f(g,c), g(d, e)]).
+add_arg_sent(a, [f(g, c, a), g(d, e)]).
*/

test(add_arg_sent) :-
    execute_test(add_arg_sent, add_arg_sent, replace_term(f(A,B), f(A,B,X)), [sentence(add_arg_sent(X, _))]).

:- end_tests(add_arg_sent).
