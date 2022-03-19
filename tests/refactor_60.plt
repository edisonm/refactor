:- begin_tests(refactor_60).

:- include(refactor_common).

/* $listtail1$
diff -ruN listtail.pl -
--- listtail.pl (source)
+++ listtail.pl (target)
@@ -1,2 +1,2 @@
-f([a, b, c /*,*/ ,  /*,*/ d, e ]).
-f([a, b, c /*,*/ |  [/*,*/ d, e ] ]).
+f([a, b, c /*,*/ ,  e, /*,*/ d ]).
+f([a, b, c /*,*/ |  [e, /*,*/ d ] ]).
*/

test(listtail1) :-
    execute_test(listtail1, replace_term([A,B], [B,A]),[file(listtail)]).

/* $listtail2$
diff -ruN listtail.pl -
--- listtail.pl (source)
+++ listtail.pl (target)
@@ -1,2 +1,2 @@
-f([a, b, c /*,*/ ,  /*,*/ d, e ]).
-f([a, b, c /*,*/ |  [/*,*/ d, e ] ]).
+f([a, b, c /*,*/ |  /*,*/ x ]).
+f([a, b, c /*,*/ |  x ]).
*/

test(listtail2) :-
    execute_test(listtail2, replace_term([_,_], x),[file(listtail)]).

/* $listtail3$
diff -ruN listtail.pl -
--- listtail.pl (source)
+++ listtail.pl (target)
@@ -1,2 +1,2 @@
-f([a, b, c /*,*/ ,  /*,*/ d, e ]).
-f([a, b, c /*,*/ |  [/*,*/ d, e ] ]).
+f([a, b, c ]).
+f([a, b, c /*,*/ |  [ ] ]).
*/

test(listtail3) :-
    execute_test(listtail3, replace_term([_,_], []),[file(listtail)]).

:- end_tests(refactor_60).
