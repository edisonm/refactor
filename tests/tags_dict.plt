:- begin_tests(tags_dict).

:- include(refactor_common).

/* $tags_dict$
diff -ruN tags_dict.pl -
--- tags_dict.pl (source)
+++ tags_dict.pl (target)
@@ -1,14 +1,14 @@
 :- module(tags_dict, [test1/0]).
 
 test1 :-
-    call(first,
+    call(second,
          [ f(o{b:1,
                a:2}
             )
          ]).
 
 test2 :-
-    call(first,
+    call(second,
          [ f(_{b:1,
                a:2}
             )
*/

test(tags_lit) :-
    execute_test(tags_dict,
                 replace_term(
                     A,
                     B,
                     ( nonvar(A),
                       A=call(first,  X),
                       B=call(second, X)
                     )),
                [file(tags_dict)]).

:- end_tests(tags_dict).
