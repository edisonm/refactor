:- begin_tests(repl_conj).

:- include(refactor_common).
:- use_module(library(call_in_dir)).

:- use_module(repl_conj).

/* $repl_conj$
diff -ruN repl_conj.pl -
--- repl_conj.pl (source)
+++ repl_conj.pl (target)
@@ -1,13 +1,11 @@
 :- module(repl_conj, [repl_conj/0]).
 
 repl_conj :-
-    a(C),
-    b(b),
+    c(C-b),
     c(C),
     d(d).
 repl_conj :-
-    a(a),
-    b(b).
+    c(a-b).
 
 a(_).
 b(_).
*/

test(repl_conj) :-
    execute_test(repl_conj, replace_conjunction(((a(A),b(B))), c(A-B))).

/* $two_changes_1$
diff -ruN repl_conj.pl -
--- repl_conj.pl (source)
+++ repl_conj.pl (target)
@@ -1,15 +1,15 @@
 :- module(repl_conj, [repl_conj/0]).
 
 repl_conj :-
-    a(C),
+    aa(C),
     b(b),
     c(C),
     d(d).
 repl_conj :-
-    a(a),
+    aa(a),
     b(b).
 
-a(_).
+aa(_).
 b(_).
 c(_).
 d(_).
*/
/* $two_changes_2$
diff -ruN repl_conj.pl -
--- repl_conj.pl (source)
+++ repl_conj.pl (target)
@@ -6,7 +6,7 @@
     c(C),
     d(d).
 repl_conj :-
-    aa(a),
+    aa(b),
     b(b).
 
 aa(_).
*/

/* $two_changes_12$
diff -ruN repl_conj.pl -
--- repl_conj.pl (source)
+++ repl_conj.pl (target)
@@ -1,15 +1,15 @@
 :- module(repl_conj, [repl_conj/0]).
 
 repl_conj :-
-    a(C),
+    aa(C),
     b(b),
     c(C),
     d(d).
 repl_conj :-
-    a(a),
+    aa(b),
     b(b).
 
-a(_).
+aa(_).
 b(_).
 c(_).
 d(_).
*/

test(two_changes) :-
    rreset,
    call_in_module_dir(repl_conj, test_two_changes).

test_two_changes :-
    replace_term(a(B),aa(B),[module(repl_conj)]),
    with_output_to(string(Result1), rshow),
    with_output_to(string(ResultD), rdiff),
    assertion(a(ResultD) == a(Result1)),
    comment_data(two_changes_1, Pattern1),
    assertion(b(Pattern1) == b(Result1)),
    replace_term(aa(a),aa(b), [module(repl_conj)]),
    with_output_to(string(Result2), rdiff),
    comment_data(two_changes_2, Pattern2),
    assertion(c(Pattern2) == c(Result2)),
    with_output_to(string(Result12), rshow),
    comment_data(two_changes_12, Pattern12),
    assertion(d(Pattern12) == d(Result12)),
    once(rundo),
    with_output_to(string(Result3), rshow),
    assertion(e(Result3)==e(ResultD)),
    rsave('/tmp/two_changes.diff'),
    delete_file('/tmp/two_changes.diff').

:- end_tests(repl_conj).
