:- begin_tests(linearize).

:- include(refactor_common).

/* $linearize1$
diff -ruN linearize.pl -
--- linearize.pl (source)
+++ linearize.pl (target)
@@ -1,3 +1,3 @@
 
-f(/* 1 */ X, /* 2 */ X).
-f(/* 1 */ X, /* 2 */ Y).
+f(/* 2 */ X, /* 1 */ X).
+f(/* 2 */ Y, /* 1 */ X).
*/

test(linearize1) :-
    execute_test(linearize1,
                 replace_term(
                     f(X,Y),
                     f(Y,X)
                 ),
                [file(linearize), linearize([vars])]).

/* $linearize2$
diff -ruN linearize.pl -
--- linearize.pl (source)
+++ linearize.pl (target)
@@ -1,3 +1,3 @@
 
 f(/* 1 */ X, /* 2 */ X).
-f(/* 1 */ X, /* 2 */ Y).
+f(/* 2 */ Y, /* 1 */ X).
*/

test(linearize2) :-
    execute_test(linearize2,
                 replace_term(
                     f(X,Y),
                     f(Y,X)
                 ),
                [file(linearize)]).

:- end_tests(linearize).
