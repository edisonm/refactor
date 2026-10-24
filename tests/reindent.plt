:- begin_tests(reindent).

:- include(refactor_common).

:- use_module(library(substitute)).
:- use_module(reindent).

/* $reindent$
diff -ruN reindent.pl -
--- reindent.pl (source)
+++ reindent.pl (target)
@@ -3,7 +3,7 @@
 p :-
     elemof(
         [ one,
-          two____, % two
+          dos, % two
           xxx,
           three,
           four
*/

test(reindent) :-
    execute_test(reindent, reindent,
                 replace_sentence(
                     (p :- Body1), (p :- Body),
                     substitute_value(two____, dos, Body1,Body)), []).

:- end_tests(reindent).
