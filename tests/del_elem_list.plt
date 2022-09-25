:- begin_tests(del_elem_list).

:- include(refactor_common).

:- use_module(del_elem_list).

/* $del_elem_list$
diff -ruN del_elem_list.pl -
--- del_elem_list.pl (source)
+++ del_elem_list.pl (target)
@@ -1,5 +1,5 @@
 :- module(del_elem_list, [del_elem_list/1]).
 
-del_elem_list([[a,b],[c,d],[e]]).
+del_elem_list([[a,b],[e]]).
 
-del_elem_list([[a,b],[c,d]]).
+del_elem_list([[a,b]]).
*/

test(del_elem_list) :-
    execute_test(del_elem_list, replace_sentence(del_elem_list([[a,b],[c,d]|T]), del_elem_list([[a,b]|T]), true)).

:- end_tests(del_elem_list).
