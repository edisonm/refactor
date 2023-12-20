:- begin_tests(op_offset).

:- include(refactor_common).

:- use_module(op_offset).

/* $op_offset$
diff -ruN op_offset.pl -
--- op_offset.pl (source)
+++ op_offset.pl (target)
@@ -2,4 +2,4 @@
           [op(980, yfx, (#))]).
 
 a#"B
-  I"+lr([]).
+  I".
*/
test(op_offset) :-
    execute_test(op_offset, op_offset, replace_term(A+lr(_), A), []).

:- end_tests(op_offset).
