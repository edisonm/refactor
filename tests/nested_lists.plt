:- begin_tests(nested_lists).

:- include(refactor_common).

:- use_module(nested_lists).

/* $nested_lists_1$
diff -ruN nested_lists.pl -
--- nested_lists.pl (source)
+++ nested_lists.pl (target)
@@ -1,3 +1,3 @@
 :- module(nested_lists, [aaa/3]).
 
-aaa([[d, _]], [/**/ d /* d */], []).
+aab([[_]], e, [d], [[c, /**/ d /* d */], [b, c, /**/ d /* d */]]).
*/

test(nested_lists_1) :-
    execute_test(nested_lists, nested_lists_1,
                 replace_sentence(aaa([[X,_]],[Y], []),
                                  aab([['$VAR'('_')]], e, [X], [[c,Y],[b,c,Y]])), [linearize([atms])]).

% Note the difference with previous test, the layout of [d] is
% preserved, due to is exactly the same term, although is via the
% variable X

/* $nested_lists_2$
diff -ruN nested_lists.pl -
--- nested_lists.pl (source)
+++ nested_lists.pl (target)
@@ -1,3 +1,3 @@
 :- module(nested_lists, [aaa/3]).
 
-aaa([[d, _]], [/**/ d /* d */], []).
+aaa([[_]], [d], [[c, /**/ d /* d */], [b, c, /**/ d /* d */]]).
*/

test(nested_lists_2) :-
    execute_test(nested_lists, nested_lists_2,
                 replace_sentence(aaa([[X,_]],[Y], []),
                                  aaa([['$VAR'('_')]], [X], [[c,Y],[b,c,Y]]), true), [linearize([atms])]).

:- end_tests(nested_lists).
