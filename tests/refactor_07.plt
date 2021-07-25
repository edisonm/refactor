:- begin_tests(refactor_07).

:- include(refactor_common).

:- use_module(ex7).

/* $ex7_1$
diff -ruN ex7.pl -
--- ex7.pl (source)
+++ ex7.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex7, [aaa/3]).
 
-aaa([[d, _]], [/**/ d /* d */], []).
+aab([[_]], e, [d], [[c, /**/ d /* d */], [b, c, /**/ d /* d */]]).
*/

test(ex7_1) :-
    execute_test(ex7, ex7_1,
                 replace_sentence(aaa([[X,_]],[Y], []),
                                  aab([['$VAR'('_')]], e, [X], [[c,Y],[b,c,Y]])), [linearize([atms])]).

% Note the difference with previous test, the layout of [d] is
% preserved, due to is exactly the same term, although is via the
% variable X

/* $ex7_2$
diff -ruN ex7.pl -
--- ex7.pl (source)
+++ ex7.pl (target)
@@ -1,3 +1,3 @@
 :- module(ex7, [aaa/3]).
 
-aaa([[d, _]], [/**/ d /* d */], []).
+aaa([[_]], [d], [[c, /**/ d /* d */], [b, c, /**/ d /* d */]]).
*/

test(ex7_2) :-
    execute_test(ex7, ex7_2,
                 replace_sentence(aaa([[X,_]],[Y], []),
                                  aaa([['$VAR'('_')]], [X], [[c,Y],[b,c,Y]]), true), [linearize([atms])]).

:- end_tests(refactor_07).
