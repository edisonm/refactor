:- begin_tests(list_tail).

:- include(refactor_common).

/* $list_tail1$
diff -ruN list_tail.pl -
--- list_tail.pl (source)
+++ list_tail.pl (target)
@@ -1,2 +1,2 @@
-f([a, b, c /*,*/ ,  /*,*/ d, e ]).
-f([a, b, c /*,*/ |  [/*,*/ d, e ] ]).
+f([a, b, c /*,*/ ,  e, /*,*/ d ]).
+f([a, b, c /*,*/ |  [e, /*,*/ d ] ]).
*/

test(list_tail1) :-
    execute_test(list_tail1, replace_term([A,B], [B,A]),[file(list_tail)]).

/* $list_tail2$
diff -ruN list_tail.pl -
--- list_tail.pl (source)
+++ list_tail.pl (target)
@@ -1,2 +1,2 @@
-f([a, b, c /*,*/ ,  /*,*/ d, e ]).
-f([a, b, c /*,*/ |  [/*,*/ d, e ] ]).
+f([a, b, c /*,*/ |  /*,*/ x ]).
+f([a, b, c /*,*/ |  x ]).
*/

test(list_tail2) :-
    execute_test(list_tail2, replace_term([_,_], x),[file(list_tail)]).

/* $list_tail3$
diff -ruN list_tail.pl -
--- list_tail.pl (source)
+++ list_tail.pl (target)
@@ -1,2 +1,2 @@
-f([a, b, c /*,*/ ,  /*,*/ d, e ]).
-f([a, b, c /*,*/ |  [/*,*/ d, e ] ]).
+f([a, b, c ]).
+f([a, b, c /*,*/ |  [ ] ]).
*/

test(list_tail3) :-
    execute_test(list_tail3, replace_term([_,_], []),[file(list_tail)]).

:- end_tests(list_tail).
