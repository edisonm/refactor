:- begin_tests(add_element_list).

:- include(refactor_common).

:- use_module(add_element_list).

/* $add_element_list$
diff -ruN add_element_list.pl -
--- add_element_list.pl (source)
+++ add_element_list.pl (target)
@@ -1,7 +1,7 @@
 :- module(add_element_list, [add_element_list/1]).
 
-add_element_list([]).
-add_element_list([/* hello */]).
-add_element_list([d]).
-add_element_list([d,e]).
-add_element_list(a).
+add_element_list([c]).
+add_element_list([c/* hello */]).
+add_element_list([c, d]).
+add_element_list([c, d,e]).
+add_element_list([c|a]).
*/

test(add_element_list) :-
    execute_test(add_element_list, replace_sentence(add_element_list(T), add_element_list([c|T]), true)).

:- end_tests(add_element_list).
